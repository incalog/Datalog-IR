package inca.debugger

import inca.backend.ir.Datalog

sealed trait Point[+T]
object Point {
  case object Before extends Point[Nothing]
  case object After extends Point[Nothing]
  case class At[T](t: T) extends Point[T]
}

object ControlPoint {
  case class AtAtom(ix: Int, before: Boolean)
  type AtomPoint = Point[AtAtom]
  case class AtBody(ix: Int, atom: AtomPoint)
  type BodyPoint = Point[AtBody]

  def patternEntryPoint(pat: Datalog.Pattern): ControlPoint = ControlPoint(pat, Point.Before)
  def patternEndPoint(pat: Datalog.Pattern): ControlPoint = ControlPoint(pat, Point.After)
  def bodyEntryPoint(pat: Datalog.Pattern, ix: Int): ControlPoint = ControlPoint(pat, Point.At(AtBody(ix, Point.Before)))
  def bodyEndPoint(pat: Datalog.Pattern, ix: Int): ControlPoint = ControlPoint(pat, Point.At(AtBody(ix, Point.After)))
  def atomEntryPoint(pat: Datalog.Pattern, bix: Int, aix: Int): ControlPoint = ControlPoint(pat, Point.At(AtBody(bix , Point.At(AtAtom(aix, before = true)))))
  def atomEndPoint(pat: Datalog.Pattern, bix: Int, aix: Int): ControlPoint = ControlPoint(pat, Point.At(AtBody(bix , Point.At(AtAtom(aix, before = false)))))
}
import ControlPoint._

case class ControlPoint(pat: Datalog.Pattern, body: BodyPoint) {
  def isPatternPoint: Boolean = body match {
    case Point.Before => true
    case _ => false
  }

  def isPatternEndPoint: Boolean = body match {
    case Point.After => true
    case _ => false
  }

  def isBodyPoint: Boolean = body match {
    case Point.At(AtBody(_, Point.Before)) => true
    case _ => false
  }

  def isBodyEndPoint: Boolean = body match {
    case Point.At(AtBody(_, Point.After)) => true
    case _ => false
  }

  def isAtomPoint: Boolean = body match {
    case Point.At(AtBody(_, Point.At(AtAtom(_, true)))) => true
    case _ => false
  }

  def isAtomEndPoint: Boolean = body match {
    case Point.At(AtBody(_, Point.At(AtAtom(_, false)))) => true
    case _ => false
  }

  def atom: Datalog.Atom = body match {
    case Point.At(AtBody(bix, Point.At(AtAtom(aix, _)))) => pat.bodies(bix).atoms(aix)
    case _ => throw new IllegalStateException(s"Cannot access atom of control point $this")
  }

  def into(implicit patterns: Map[String, Datalog.Pattern]): Option[ControlPoint] = body match {
    case Point.Before =>
      Some(ControlPoint.bodyEntryPoint(pat, 0))
    case Point.After =>
      None
    case Point.At(AtBody(bix, Point.Before)) =>
      Some(ControlPoint.atomEntryPoint(pat, bix, 0))
    case Point.At(AtBody(bix, Point.After)) =>
      if (pat.bodies.size <= bix + 1)
        Some(ControlPoint.patternEndPoint(pat))
      else
        Some(ControlPoint.bodyEntryPoint(pat, bix + 1))
    case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
      val atom = pat.bodies(bix).atoms(aix)
      atom match {
        case Datalog.Call(name, _, _, _) =>
          Some(ControlPoint.patternEntryPoint(patterns(name)))
        case _ =>
          Some(ControlPoint.atomEndPoint(pat, bix, aix))
      }
    case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
      if (pat.bodies(bix).atoms.size <= aix + 1)
        Some(ControlPoint.bodyEndPoint(pat, bix))
      else
        Some(ControlPoint.atomEntryPoint(pat, bix, aix + 1))
  }

  def over: Option[ControlPoint] = body match {
    case Point.Before =>
      Some(ControlPoint.patternEndPoint(pat))
    case Point.After =>
      None
    case Point.At(AtBody(bix, Point.Before)) =>
      Some(ControlPoint.bodyEndPoint(pat, bix))
    case Point.At(AtBody(bix, Point.After)) =>
      Some(ControlPoint.patternEndPoint(pat))
    case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
      Some(ControlPoint.atomEndPoint(pat, bix, aix))
    case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
      if (pat.bodies(bix).atoms.size <= aix + 1)
        Some(ControlPoint.bodyEndPoint(pat, bix))
      else
        Some(ControlPoint.atomEntryPoint(pat, bix, aix + 1))
  }

  def out: Option[ControlPoint] = body match {
    case Point.Before =>
      Some(ControlPoint.patternEndPoint(pat))
    case Point.After =>
      None
    case Point.At(AtBody(bix, Point.Before)) =>
      Some(ControlPoint.bodyEndPoint(pat, bix))
    case Point.At(AtBody(bix, Point.After)) =>
      Some(ControlPoint.patternEndPoint(pat))
    case Point.At(AtBody(bix, Point.At(_))) =>
      Some(ControlPoint.bodyEndPoint(pat, bix))
  }
}
