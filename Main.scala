//> using scala "3.2"
//> using dep "org.http4s::http4s-core:0.23.19"
//> using dep "org.http4s::http4s-ember-server:0.23.19"

package hlinx

import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.port
import org.http4s.*
import org.http4s.implicits.*
import org.http4s.ember.server.EmberServerBuilder

import scala.concurrent.duration.*

object Main extends IOApp {
  case class Greeter(who: String)


  override def run(args: List[String]): IO[ExitCode] = {
    EmberServerBuilder.default[IO]
      .withPort(port"8080")
      .withHttpApp(
        Routed.builder[IO, Unit](Root / "hello" / param[String]("who"))
          .get(
            ContextRoutes {
              case ContextRequest(((), ctx), req) =>
                val greeter = ctx.to[Greeter]
                OptionT.some[IO](Response[IO]().withEntity(s"Hello ${greeter.pathType.who}"))
            }
          ).buildHttpRoutes.orNotFound
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
