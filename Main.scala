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

  override def run(args: List[String]): IO[ExitCode] = {
    val builder = Routed.httpRoutes[IO]
      .path(Root / "hello" / param[String]("who"))(
        _.get(
          Handler {
            case ContextRequest(ctx, req) =>
              //val greeter = ctx.to[Greeter]
              //println(greeter)
              IO(Response[IO]().withEntity(s"Hello ${ctx.linx}"))
          })
      )
      .path(Root / "hello" / "world")(
        _.get(
          Handler {
            case ContextRequest(ctx, req) =>
              IO(Response[IO]().withEntity(s"Hello World"))
          })
      )
    println(builder)
    EmberServerBuilder.default[IO]
      .withPort(port"8080")
      .withHttpApp(
        ErrorHandling.Custom.recoverWith(builder.buildHttpRoutes.orNotFound) {
          case t =>
            t.printStackTrace()
            IO(Response(Status.InternalServerError))
        }
      )
      .build.useForever
  }


  /*val path = Root / "foo" / param[String]("bar") / param[Int]("baz")
  val zz = hlinx("foo", param[String]("bar"), param[Int]("baz"))
  //p"foo/${bar}/${baz}"
  //println(zz.template)
  println(path.asList)
  val matchingpath = uri"/foo/bar/134"
  println(path.to[Foo](matchingpath.path))*/


}
