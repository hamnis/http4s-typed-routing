package hlinx

import cats.Monad
import cats.syntax.all.*
import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import org.http4s.Uri.Path
import org.http4s.headers.Allow
import org.http4s.*

import scala.deriving.Mirror

type RoutedRequest[F[_], A, T] = ContextRequest[F, (A, RouteContext[T])]
type Routed[F[_], A, T] = ContextRoutes[(A, RouteContext[T]), F]

object Routed {
  def of[F[_] : Monad, A, T <: Tuple](template: HLinx[T], methods: Map[Method, Routed[F, A, T]]): ContextRoutes[A, F] =
    ContextRoutes[A, F] {
      case ContextRequest(a, req) =>
        val path = req.pathInfo
        template.extract(path) match
          case Left(value) => OptionT.none //todo: this needs work
          case Right(value) => {
            val route: Routed[F, A, T] = methods.getOrElse(
              req.method,
              ContextRoutes(
                _ => OptionT.some[F](Response[F](Status.MethodNotAllowed).putHeaders(Allow(methods.keySet)))
              )
            )
            val templateContext = RouteContext(value, template.template)
            route(ContextRequest(a -> templateContext, req))
          }
    }

  def builder[F[_], A] = BuilderStep0[F, A]()

  def httpRoutes[F[_]] = BuilderStep0[F, Unit]()

  case class BuilderStep0[F[_], A]() {
    def apply[T <: Tuple](linx: HLinx[T]): BuilderStep1[F, A, T] = BuilderStep1(linx)
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

    def build(using M: Monad[F]): ContextRoutes[A, F] = Routed.of(template, methods)

    def buildHttpRoutes(using ev: Unit =:= A, M: Monad[F]): HttpRoutes[F] = {
      val route = Routed.of(template, methods)
      Kleisli { req =>
        route(ContextRequest(ev(()), req))
      }
    }
  }
}

case class RouteContext[T](context: T, template: Template) {
  inline def to[A](using m: Mirror.ProductOf[A]): RouteContext[A] =
    context match
      case t: m.MirroredElemTypes => RouteContext(m.fromTuple(t), template)
}
