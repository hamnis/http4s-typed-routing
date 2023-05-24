//> using scala "3.2"
//> using dep "org.http4s::http4s-core:0.23.19"
//> using dep "org.http4s::http4s-ember-server:0.23.19"
//> using dep "org.http4s::http4s-dsl:0.23.19"
//> using dep "org.slf4j:slf4j-simple:2.0.7"

package hlinx

import cats.arrow.FunctionK
import cats.{Monad, ~>}
import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.port
import org.http4s.*
import org.http4s.dsl.impl.{Responses, Statuses}
import org.http4s.implicits.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.ErrorHandling

import scala.concurrent.duration.*

object Main extends IOApp with ContextRoutesBuilderDsl[IO, Unit] {
  case class Greeter(who: String)

  override def run(args: List[String]): IO[ExitCode] = {
    val b = newBuilder
      .path[Greeter](Root / "hello" / param[String]("who"))(
        _.get(
          r => Ok(s"Hello ${r.context.linx.who}")
        ).post(
          req =>
            req.as[String].flatMap(msg =>
              Ok(s"Hello ${req.context.linx.who}, with $msg")
            )
        )
      )
      .path(Root / "hello" / "world")(
        _.get(
          _ => Ok(s"Hello World")
        )
      )
    b.report >>
      EmberServerBuilder.default[IO]
        .withPort(port"8080")
        .withHttpApp(
          b.buildHttpRoutes.orNotFound
        )
        .build.useForever
  }
}
