package hlinx

import cats.*
import cats.syntax.all.*
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.effect.std.Console
import org.http4s.Uri.Path
import org.http4s.headers.Allow
import org.http4s.*

import scala.deriving.Mirror

case class RouteContext[T, C](context: C, linx: T, template: Template)

case class Route[F[_], A](template: Template, methods: Set[Method], route: ContextRoutes[A, F]) {
  def report = s"${template.asString} (${methods.toList.sorted(Method.catsInstancesForHttp4sMethod.toOrdering).mkString(",")})"
}

type Routed[F[_], A, T] = ContextRoutes[RouteContext[T, A], F]

object Routed {
  def compile[F[_] : Monad, A, T <: Tuple, S](template: HLinx[T], methods: Map[Method, Routed[F, A, S]], toS: T => S): Route[F, A] =
    Route(template.template, methods.keySet, full[F, A] {
      case ContextRequest(a, req) =>
        val path = req.pathInfo
        template.extract(path) match
          case Left(CaptureFailure.NotFound) => OptionT.none
          case Left(m: CaptureFailure.MissingPathParam) =>
            OptionT.some[F](Response[F](Status.BadRequest).withEntity(m.errorMessage))
          case Left(m: CaptureFailure.PathParamConvertFailure) =>
            OptionT.some[F](Response[F](Status.BadRequest).withEntity(m.errorMessage)) //todo: this needs work
          case Right(value) => {
            val route: Routed[F, A, S] =
              methods.getOrElse(
                req.method,
                Routed.pure(
                  Response[F](Status.MethodNotAllowed).putHeaders(Allow(methods.keySet))
                )
              )
            val templateContext = RouteContext(a, toS(value), template.template)
            val previous = req.attributes.lookup(Request.Keys.PathInfoCaret).getOrElse(0)

            route(ContextRequest(templateContext, req.withAttribute(Request.Keys.PathInfoCaret, previous + path.segments.size)))
          }
    })

  def apply[F[_], A, T](run: ContextRequest[F, RouteContext[T, A]] => F[Response[F]])(using Functor[F]): Routed[F, A, T] = Kleisli(run).mapF(OptionT.liftF)

  private[hlinx] def full[F[_], A](run: ContextRequest[F, A] => OptionT[F, Response[F]])(using Functor[F]): ContextRoutes[A, F] = Kleisli(run)

  def pure[F[_], A, T](response: Response[F])(using Applicative[F]): Routed[F, A, T] = Kleisli(_ => OptionT.some[F](response))

  def builder[F[_], A](using Monad[F]) = BuilderStep0[F, A](Nil)

  def httpRoutes[F[_]](using Monad[F]) = BuilderStep0[F, Unit](Nil)

  case class BuilderStep0[F[_], A](routes: List[Route[F, A]])(using Monad[F]) {
    def dynamic[T <: Tuple](linx: HLinx[T])(f: BuilderStep1[F, A, T, T] => BuilderStep2[F, A, T, T]): BuilderStep0[F, A] =
      BuilderStep0(f(BuilderStep1(linx)).build(identity) :: routes)

    inline def dynamic[S <: Product](using mp: Mirror.ProductOf[S])(linx: HLinx[mp.MirroredElemTypes])(f: BuilderStep1[F, A, mp.MirroredElemTypes, S] => BuilderStep2[F, A, mp.MirroredElemTypes, S]): BuilderStep0[F, A] =
      BuilderStep0(f(BuilderStep1(linx)).build(mp.fromTuple) :: routes)

    def static(linx: HLinx[EmptyTuple])(f: BuilderStep1[F, A, EmptyTuple, EmptyTuple] => BuilderStep2[F, A, EmptyTuple, EmptyTuple]) =
      BuilderStep0(f(BuilderStep1(linx)).build(identity) :: routes)

    def build(using M: Monad[F]): ContextRoutes[A, F] = {
      routes.sortBy(_.template).map(_.route: ContextRoutes[A, F]).foldK
    }

    def report(using C: Console[F]) = C.println("Registered routes:") >> C.println(routes.map(_.report).mkString("\n"))

    def buildHttpRoutes(using ev: Unit =:= A, M: Monad[F]): HttpRoutes[F] = {
      val sorted = build
      Kleisli { req =>
        sorted.run(ContextRequest(ev(()), req))
      }
    }
  }

  sealed trait WithBuilderStep2[F[_], A, T <: Tuple, S <: Product] {
    def withMethod(method: Method, routed: Routed[F, A, S]): BuilderStep2[F, A, T, S]

    def get(routed: Routed[F, A, S]) = withMethod(Method.GET, routed)

    def post(routed: Routed[F, A, S]) = withMethod(Method.POST, routed)

    def put(routed: Routed[F, A, S]) = withMethod(Method.PUT, routed)

    def delete(routed: Routed[F, A, S]) = withMethod(Method.DELETE, routed)
  }

  case class BuilderStep1[F[_], A, T <: Tuple, S <: Product](template: HLinx[T]) extends WithBuilderStep2[F, A, T, S] {
    def withMethod(method: Method, routed: Routed[F, A, S]) = BuilderStep2(template, Map(method -> routed))
  }

  case class BuilderStep2[F[_], A, T <: Tuple, S <: Product](template: HLinx[T], methods: Map[Method, Routed[F, A, S]]) extends WithBuilderStep2[F, A, T, S] {
    def withMethod(method: Method, routed: Routed[F, A, S]) = BuilderStep2(template, methods.updated(method, routed))

    def build(toS: T => S)(using m: Monad[F]): Route[F, A] = Routed.compile(template, methods, toS)
  }
}
