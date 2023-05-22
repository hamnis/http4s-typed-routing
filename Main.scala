//> using scala "3.2"
//> using dep "org.http4s::http4s-core:0.23.19"

package hlinx

import cats.data.{Kleisli, OptionT}
import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.*
import org.http4s.implicits.*

import scala.concurrent.duration.*

object Main {
  case class Foo(bar: String, baz: Int)

  def main(args: Array[String]): Unit = {
    val path = Root / "foo" / param[String]("bar") / param[Int]("baz")
    val zz = hlinx("foo", param[String]("bar"), param[Int]("baz"))
    //p"foo/${bar}/${baz}"
    //println(zz.template)
    println(path.asList)
    val matchingpath = uri"/foo/bar/134"
    println(path.to[Foo](matchingpath.path))

  }
}
