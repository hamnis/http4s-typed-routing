package hlinx

import cats.*
import cats.syntax.all.*
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import org.http4s.Uri.Path
import org.http4s.headers.Allow
import org.http4s.*

import scala.deriving.Mirror

case class RouteContext[T, C](context: C, linx: T, template: Template)

type Handler[F[_], A] = ContextRoutes[A, F]

object Handler {
  def apply[F[_], A](run: ContextRequest[F, A] => F[Response[F]])(using Functor[F]): Handler[F, A] = Kleisli(run).mapF(OptionT.liftF)

  case class TypedPartiallyApplied[F[_], A]()(using F: Functor[F]) {
    inline def apply[S <: Product](run: ContextRequest[F, RouteContext[S, A]] => F[Response[F]])(using m: Mirror.ProductOf[S]) = {
      Handler.full[F, RouteContext[m.MirroredElemTypes, A]] {
        case r@ContextRequest(ctx, req) => {
          ctx.linx match {
            case t: m.MirroredElemTypes =>
              val newReq = ContextRequest(RouteContext(ctx.context, m.fromTuple(t), ctx.template), req)
              OptionT.liftF(run(newReq))
          }
        }
      }
    }
  }

  def typed[F[_], A](using Functor[F]) = TypedPartiallyApplied[F, A]()

  /*inline def typed[F[_], A, T, S <: Product](run: ContextRequest[F, RouteContext[S, A]] => F[Response[F]])(using f: Functor[F], m: Mirror.ProductOf[S]): Handler[F, RouteContext[T, A]] =
    Handler.full[F, RouteContext[T, A]] {
      case r@ContextRequest(ctx, req) => {
        ctx.linx match {
          case t: m.MirroredElemTypes =>
            val newReq = ContextRequest(RouteContext(ctx.context, m.fromTuple(t), ctx.template), req)
            OptionT.liftF(run(newReq))
        }
      }
    }*/

  def full[F[_], A](run: ContextRequest[F, A] => OptionT[F, Response[F]])(using Functor[F]): Handler[F, A] = Kleisli(run)

  def pure[F[_], A](response: Response[F])(using Applicative[F]): Handler[F, A] = Kleisli(_ => OptionT.some[F](response))
}

case class Route[F[_], A](template: Template, route: Handler[F, A])

type RoutedRequest[F[_], A, T] = ContextRequest[F, RouteContext[T, A]]
type Routed[F[_], A, T] = Handler[F, RouteContext[T, A]]
extension[F[_], A, T <: Tuple] (outer: Routed[F, A, T])(using Functor[F]) {
  inline def toTyped[S <: Product](using m: Mirror.ProductOf[S]): Routed[F, A, S] =
    Handler.full[F, RouteContext[S, A]] {
      case r@ContextRequest(ctx, req) => {
        val newReq = ContextRequest(RouteContext(ctx.context, Tuple.fromProduct(ctx.linx).asInstanceOf[T], ctx.template), req)
        outer(newReq)
      }
    }
}

object Routed {
  def compile[F[_] : Monad, A, T <: Tuple](template: HLinx[T], methods: Map[Method, Routed[F, A, T]]): Route[F, A] =
    Route(template.template, Handler.full[F, A] {
      case ContextRequest(a, req) =>
        val path = req.pathInfo
        template.extract(path) match
          case Left(CaptureFailure.NotFound) => OptionT.none
          case Left(m: CaptureFailure.MissingPathParam) =>
            OptionT.some[F](Response[F](Status.BadRequest).withEntity(m.errorMessage))
          case Left(m: CaptureFailure.PathParamConvertFailure) =>
            OptionT.some[F](Response[F](Status.BadRequest).withEntity(m.errorMessage)) //todo: this needs work
          case Right(value) => {
            val route: Routed[F, A, T] =
              methods.getOrElse(
                req.method,
                Handler.pure(
                  Response[F](Status.MethodNotAllowed).putHeaders(Allow(methods.keySet))
                )
              )
            val templateContext = RouteContext(a, value, template.template)
            route(ContextRequest(templateContext, req))
          }
    })

  def builder[F[_], A](using Monad[F]) = BuilderStep0[F, A](Nil)

  def httpRoutes[F[_]](using Monad[F]) = BuilderStep0[F, Unit](Nil)

  case class BuilderStep0[F[_], A](routes: List[Route[F, A]])(using Monad[F]) {
    def path[T <: Tuple](linx: HLinx[T])(f: BuilderStep1[F, A, T] => BuilderStep2[F, A, T]): BuilderStep0[F, A] =
      copy(f(BuilderStep1(linx)).build :: routes)

    def build(using M: Monad[F]): ContextRoutes[A, F] = {
      routes.sortBy(_.template).map(_.route: ContextRoutes[A, F]).foldK
    }

    def buildHttpRoutes(using ev: Unit =:= A, M: Monad[F]): HttpRoutes[F] = {
      val sorted = build
      Kleisli { req =>
        sorted.run(ContextRequest(ev(()), req))
      }
    }
  }

  sealed trait WithBuilderStep2[F[_], A, T <: Tuple] {
    def withMethod(method: Method, routed: Routed[F, A, T]): BuilderStep2[F, A, T]

    def get(routed: Routed[F, A, T]) = withMethod(Method.GET, routed)

    def post(routed: Routed[F, A, T]) = withMethod(Method.POST, routed)

    def put(routed: Routed[F, A, T]) = withMethod(Method.PUT, routed)

    def delete(routed: Routed[F, A, T]) = withMethod(Method.DELETE, routed)
  }

  case class BuilderStep1[F[_], A, T <: Tuple](template: HLinx[T]) extends WithBuilderStep2[F, A, T] {
    def withMethod(method: Method, routed: Routed[F, A, T]) = BuilderStep2(template, Map(method -> routed))
  }

  case class BuilderStep2[F[_], A, T <: Tuple](template: HLinx[T], methods: Map[Method, Routed[F, A, T]]) extends WithBuilderStep2[F, A, T] {
    def withMethod(method: Method, routed: Routed[F, A, T]) = BuilderStep2(template, methods.updated(method, routed))

    def build(using M: Monad[F]): Route[F, A] = Routed.compile(template, methods)
  }
}
