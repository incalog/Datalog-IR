package inca.debugger.old

import inca.backend.ir.Datalog

object ControlPoint {
  def patternEntry(pat: Datalog.Pattern): ControlPoint =
    ControlPoint(PatternPoint(pat, BeforeList))
  def patternExit(pat: Datalog.Pattern): ControlPoint =
    ControlPoint(PatternPoint(pat, AfterList))
}

case class BeforeAfter[P](point: P, before: Boolean)
object Before {
  def apply[P](p: P): BeforeAfter[P] = BeforeAfter(p, before = true)
  def unapply[P](ba: BeforeAfter[P]): Option[P] =
    if (ba.before)
      Some(ba.point)
    else
      None
}
object After {
  def apply[P](p: P): BeforeAfter[P] = BeforeAfter(p, before = false)
  def unapply[P](ba: BeforeAfter[P]): Option[P] =
    if (ba.before)
      None
    else
      Some(ba.point)
}

sealed trait ListPoint[+El, +P]
case object BeforeList extends ListPoint[Nothing, Nothing]
case object AfterList extends ListPoint[Nothing, Nothing]
case class AtListElem[El, P](elems: IndexedSeq[El], ix: Int, point: P) extends ListPoint[El, P] {
  override def toString: String = s"AtListElem($ix, $point)"

  val elem: El = elems(ix)

  def isLast: Boolean =
    ix + 1 == elems.size
  def hasNext: Boolean =
    ix + 1 < elems.size
  def getNext: Option[El] = {
    val next = ix + 1
    elems.lift(next)
  }
  def next(f: El => P): ListPoint[El, P] = {
    val nextIx = ix + 1
    if (nextIx < elems.size)
      AtListElem(elems, nextIx, f(elems(nextIx)))
    else
      AfterList
  }
}
object ListPoint {
  def first[El, P](elems: Seq[El], f: El => P): ListPoint[El, P] =
    if (elems.isEmpty)
      AfterList
    else
      AtListElem(elems.toIndexedSeq, 0, f(elems.head))
}

case class AtomPoint(atom: Datalog.Atom)
case class BodyPoint(body: Datalog.Body, atoms: ListPoint[Datalog.Atom, AtomPoint]) {
  override def toString: String = s"BodyPoint($atoms)"

  def atom: Option[Datalog.Atom] = atoms match {
    case elem @ AtListElem(_, _, _) => Some(elem.elem)
    case _ => None
  }
  def stepIntra: Option[BodyPoint] = atoms match {
    case BeforeList => Some(BodyPoint(body, ListPoint.first(body.atoms, AtomPoint.apply)))
    case elem @ AtListElem(_, _, _) => Some(BodyPoint(body, elem.next(AtomPoint.apply)))
    case AfterList => None
  }
  def stepOver: Option[BodyPoint] = atoms match {
    case BeforeList => Some(BodyPoint(body, AfterList))
    case elem @ AtListElem(_, _, _) => Some(BodyPoint(body, elem.next(AtomPoint.apply)))
    case AfterList => None
  }
  def stepOut: Option[BodyPoint] = atoms match {
    case BeforeList | AtListElem(_, _, _) => Some(BodyPoint(body, AfterList))
    case AfterList => None
  }
  def abortBody: BodyPoint = BodyPoint(body, AfterList)
  def isBodyEntry: Boolean = atoms == BeforeList
  def isBodyExit: Boolean = atoms == AfterList
}
case class PatternPoint(pat: Datalog.Pattern, bodies: ListPoint[Datalog.Body, BodyPoint]) {
  override def toString: String = s"PatternPoint(${pat.name}, $bodies)"

  def atom: Option[Datalog.Atom] = bodies match {
    case AtListElem(_, _, bodyPoint) => bodyPoint.atom
    case _ => None
  }
  def body: Option[Datalog.Body] = bodies match {
    case AtListElem(bodies, ix, _) => Some(bodies(ix))
    case _ => None
  }
  def stepIntra: Option[PatternPoint] = bodies match {
    case BeforeList =>
      Some(PatternPoint(pat, ListPoint.first(pat.bodies, BodyPoint(_, BeforeList))))
    case elem @ AtListElem(_, _, body) =>
      body.stepIntra match {
        case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
        case None => Some(PatternPoint(pat, elem.next(BodyPoint(_, BeforeList))))
      }
    case AfterList => None
  }
  def stepOver: Option[PatternPoint] = bodies match {
    case BeforeList => Some(PatternPoint(pat, AfterList))
    case elem @ AtListElem(_, _, body) =>
      body.stepOver match {
        case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
        case None => None
      }
    case AfterList => None
  }
  def stepOut: Option[PatternPoint] = bodies match {
    case BeforeList => Some(PatternPoint(pat, AfterList))
    case elem @ AtListElem(_, _, body) =>
      body.stepOut match {
        case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
        case None => Some(PatternPoint(pat, AfterList))
      }
    case AfterList => None
  }
  def abortBody: PatternPoint = bodies match {
    case BeforeList | AfterList => this
    case AtListElem(elems, ix, point) => PatternPoint(pat, AtListElem(elems, ix, point.abortBody))
  }
  def isPatternEntry: Boolean = bodies == BeforeList
  def isPatternExit: Boolean = bodies == AfterList
  def isBodyEntry: Boolean = bodies match {
    case AtListElem(_, _, b) => b.isBodyEntry
    case _ => false
  }
  def isBodyExit: Boolean = bodies match {
    case AtListElem(_, _, b) => b.isBodyExit
    case _ => false
  }
  def bodyIndex: Option[Int] = bodies match {
    case AtListElem(_, ix, _) => Some(ix)
    case _ => None
  }
}

case class ControlPoint(point: PatternPoint) {
  def stepIntra: Option[ControlPoint] = point.stepIntra.map(ControlPoint.apply)
  def stepOver: Option[ControlPoint] = point.stepOver.map(ControlPoint.apply)
  def stepOut: Option[ControlPoint] = point.stepOut.map(ControlPoint.apply)
  def abortBody: ControlPoint = ControlPoint(point.abortBody)

  def isPatternEntry: Boolean = point.isPatternEntry
  def isPatternExit: Boolean = point.isPatternExit
  def isBodyEntry: Boolean = point.isBodyEntry
  def isBodyExit: Boolean = point.isBodyExit
  def isAtomPoint: Boolean = point.bodies match {
    case AtListElem(_, _, BodyPoint(_, AtListElem(_, _, _))) => true
    case _ => false
  }

  def atom: Datalog.Atom =
    point.atom.getOrElse(
      throw new IllegalStateException(s"Cannot access atom of control point $this")
    )
}
