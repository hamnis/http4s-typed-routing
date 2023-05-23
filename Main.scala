//> using scala "3.2"
//> using dep "org.http4s::http4s-core:0.23.19"
//> using dep "org.http4s::http4s-ember-server:0.23.19"
//> using dep "org.slf4j:slf4j-simple:2.0.7"

package hlinx

import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.port
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling

import scala.concurrent.duration.*

object Main extends IOApp {
  case class Greeter(who: String)

  given[F[_], A]: Conversion[ContextRequest[F, A], Request[F]] = _.req

  override def run(args: List[String]): IO[ExitCode] = {
    val getDynamicRoute = Routed.httpRoutes[IO, Greeter] {
      r => IO(Response[IO]().withEntity(s"Hello ${r.context.linx.who}"))
    }

    val builder = Route.httpRoutes[IO]
      .dynamic[Greeter](Root / "hello" / param[String]("who"))(
        _.get(getDynamicRoute).post(
          Routed {
            req =>
              req.as[String].flatMap(msg =>
                IO(Response[IO]().withEntity(s"Hello ${req.context.linx.who}, with $msg"))
              )
          }
        )
      )
      .static(Root / "hello" / "world")(
        _.get(
          Routed.response(Response[IO]().withEntity(s"Hello World")))
      )
    builder.report >>
      EmberServerBuilder.default[IO]
        .withPort(port"8080")
        .withHttpApp(
          builder.buildHttpRoutes.orNotFound
        )
        .build.useForever
  }
}
