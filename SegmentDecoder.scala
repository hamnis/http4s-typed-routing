package hlinx

import org.http4s.Uri

@FunctionalInterface
trait SegmentDecoder[A] {
  self =>
  def decode(segment: Uri.Path.Segment): Either[String, A]

  final def map[B](f: A => B): SegmentDecoder[B] = (s) => self.decode(s).map(f)

  final def flatMap[B](f: A => SegmentDecoder[B]): SegmentDecoder[B] = (s) => self.decode(s).flatMap(a => f(a).decode(s))
}

object SegmentDecoder {
  given SegmentDecoder[Int] = (seg) => seg.decoded().toIntOption.toRight(s"'$seg' is not a valid int")

  given SegmentDecoder[Long] = (seg) => seg.decoded().toLongOption.toRight(s"'$seg' is not a valid long")

  given SegmentDecoder[String] = (seg) => Right(seg.decoded())

  given SegmentDecoder[Unit] = (_) => Right(())
}
