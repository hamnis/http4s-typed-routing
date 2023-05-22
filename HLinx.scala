package hlinx

import org.http4s.{EmptyBody, Uri}

import scala.annotation.tailrec
import scala.compiletime.summonInline
import scala.deriving.Mirror

enum CaptureFailure {
  def errorMessage: String = this match {
    case MissingPathParam(name) => s"Missing required path parameter [$name]"
    case MissingStaticPath(name) => s"Missing required static path segment [$name]"
    case PathParamConvertFailure(name, msg) => s"Could not convert path parameter $name"
    case EmptyPath => "Could not match on an empty path"
  }

  case MissingPathParam(name: String)
  case MissingStaticPath(name: String)
  case EmptyPath
  case PathParamConvertFailure(name: String, error: String)
}

enum SegmentType {
  case Static(value: String)
  case Variable(value: String)
}

object SegmentType {
  given Ordering[SegmentType] = Ordering.by {
    case s: SegmentType.Static => 1
    case v: SegmentType.Variable => 2
  }
}

case class Param[A](name: SegmentType.Variable, converter: SegmentDecoder[A])

def param[A](name: String)(using S: SegmentDecoder[A]) = Param(SegmentType.Variable(name), S)

case class Template(value: List[SegmentType]) {
  def asString: String = value.map {
    case SegmentType.Static(value) => value
    case SegmentType.Variable(value) => s"{$value}"
  }.mkString("/", "/", "")
}

object Template {
  given Ordering[Template] = Ordering.by[Template, List[SegmentType]](_.value)(Ordering.Implicits.seqOrdering)
}

sealed trait HLinx[T <: Tuple] {
  def extract(uri: Uri.Path): Either[CaptureFailure, T] =
    extractImpl(uri.segments.toList.reverse)

  lazy val template = Template({
    @tailrec def go(e: HLinx[_], list: List[SegmentType]): List[SegmentType] =
      e match {
        case Root => list
        case Static(parent, name) => go(parent, name :: list)
        case Variable(parent, name, _) => go(parent, name :: list)
      }

    go(this, Nil)
  })

  private[hlinx] def extractImpl(path: List[Uri.Path.Segment]): Either[CaptureFailure, T]

  inline def to[A](path: Uri.Path)(using m: Mirror.ProductOf[A]): Either[CaptureFailure, A] = {
    extract(path).map {
      case t: m.MirroredElemTypes =>
        m.fromTuple(t)
    }
  }

  def /(segment: String) = Static(this, SegmentType.Static(segment))

  def /[A](param: Param[A]) = Variable(this, param.name, param.converter)
}

case object Root extends HLinx[EmptyTuple] {
  private[hlinx] override def extractImpl(path: List[Uri.Path.Segment]): Either[CaptureFailure, EmptyTuple] =
    if (path.isEmpty) Right(EmptyTuple) else Left(CaptureFailure.EmptyPath)
}

case class Static[T <: Tuple](parent: HLinx[T], name: SegmentType.Static) extends HLinx[T] {
  private object CurrentSegment {
    def unapply(seg: Uri.Path.Segment): Option[String] = {
      val str = seg.decoded()
      Option.when(str == name.value)(str)
    }
  }

  private[hlinx] override def extractImpl(path: List[Uri.Path.Segment]): Either[CaptureFailure, T] = {
    path match
      case CurrentSegment(head) :: rest =>
        parent.extractImpl(rest)
      case _ => Left(CaptureFailure.MissingStaticPath(name.value))
  }
}

case class Variable[H, T <: Tuple](parent: HLinx[T], name: SegmentType.Variable, converter: SegmentDecoder[H]) extends HLinx[Tuple.Append[T, H]] {
  private[hlinx] override def extractImpl(path: List[Uri.Path.Segment]): Either[CaptureFailure, Tuple.Append[T, H]] =
    path match
      case head :: tail =>
        parent.extractImpl(tail).flatMap((tuple: T) =>
          converter.decode(head).left.map(err => CaptureFailure.PathParamConvertFailure(name.value, err))
            .map(hDec => tuple :* hDec)
        )
      case _ => Left(CaptureFailure.MissingPathParam(name.value))
}