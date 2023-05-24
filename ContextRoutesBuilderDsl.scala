package hlinx

import cats.arrow.FunctionK
import cats.{Monad, ~>}
import org.http4s.{ContextRequest, Request}
import org.http4s.dsl.impl.{Responses, Statuses}

trait ContextRoutesBuilderDsl[F[_] : Monad, A] extends Statuses with Responses[F, F] {
  override final val liftG: F ~> F = FunctionK.id[F]

  given[G[_], B]: Conversion[ContextRequest[G, B], Request[G]] = _.req

  def newBuilder = Route.builder[F, A]
}
