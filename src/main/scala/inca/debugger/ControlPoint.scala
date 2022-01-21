package inca.debugger

import inca.backend.ir.Datalog


object ControlPoint {
//  case class AtAtom(ix: Int, before: Boolean)
//  type AtomPoint = Point[AtAtom]
//  case class AtBody(ix: Int, atom: AtomPoint)


  def patternEntryPoint(pat: Datalog.Pattern): ControlPoint =
    ControlPoint(PatternPoint(pat, BeforeList))
//  def patternEndPoint(pat: Datalog.Pattern): ControlPoint =
//    ControlPoint(PatternPoint(pat, AfterList))
//  def bodyEntryPoint(pat: Datalog.Pattern, ix: Int): ControlPoint =
//    ControlPoint(PatternPoint(pat, AtListElem(pat.bodies.toIndexedSeq, ix, BodyPoint(BeforeList))))
//  def bodyEndPoint(pat: Datalog.Pattern, ix: Int): ControlPoint =
//    ControlPoint(PatternPoint(pat, AtListElem(pat.bodies.toIndexedSeq, ix, BodyPoint(AfterList))))
//  def atomEntryPoint(body: Datalog.Body, aix: Int): BodyPoint = BodyPoint(AtListElem(body.atoms.toIndexedSeq, aix, Before(AtomPoint)))
//  def atomEndPoint(pat: Datalog.Pattern, bix: Int, aix: Int): ControlPoint = {
//    val bodies = pat.bodies.toIndexedSeq
//    ControlPoint(PatternPoint(pat, AtListElem(bodies, bix, BodyPoint(AtListElem(bodies(bix).atoms.toIndexedSeq, aix, After(AtomPoint))))))
//  }
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
    case elem@AtListElem(_, _, _) => Some(elem.elem)
    case _ => None
  }
  def stepIntra: Option[BodyPoint] = atoms match {
    case BeforeList => Some(BodyPoint(body, ListPoint.first(body.atoms, AtomPoint.apply)))
    case elem@AtListElem(_, _, _) => Some(BodyPoint(body, elem.next(AtomPoint.apply)))
    case AfterList => None
  }
  def stepOver: Option[BodyPoint] = atoms match {
    case BeforeList => Some(BodyPoint(body, AfterList))
    case elem@AtListElem(_, _, _) => Some(BodyPoint(body, elem.next(AtomPoint.apply)))
    case AfterList => None
  }
  def stepOut: Option[BodyPoint] = atoms match {
    case BeforeList | AtListElem(_, _, _) => Some(BodyPoint(body, AfterList))
    case AfterList => None
  }
  def isBodyEntry: Boolean = atoms == BeforeList
  def isBodyExit: Boolean = atoms == AfterList
}
case class PatternPoint(pat: Datalog.Pattern, bodies: ListPoint[Datalog.Body, BodyPoint]) {
  override def toString: String = s"PatternPoint(${pat.name}, $bodies)"

  def atom: Option[Datalog.Atom] = bodies match {
    case AtListElem(_, _, bodyPoint) => bodyPoint.atom
    case _ => None
  }
  def stepIntra: Option[PatternPoint] = bodies match {
    case BeforeList => Some(PatternPoint(pat, ListPoint.first(pat.bodies, BodyPoint(_, BeforeList))))
    case elem@AtListElem(_, _, body) => body.stepIntra match {
      case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
      case None => Some(PatternPoint(pat, elem.next(BodyPoint(_, BeforeList))))
    }
    case AfterList => None
  }
  def stepOver: Option[PatternPoint] = bodies match {
    case BeforeList => Some(PatternPoint(pat, AfterList))
    case elem@AtListElem(_, _, body) => body.stepOver match {
      case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
      case None => None
    }
    case AfterList => None
  }
  def stepOut: Option[PatternPoint] = bodies match {
    case BeforeList => Some(PatternPoint(pat, AfterList))
    case elem@AtListElem(_, _, body) => body.stepOut match {
      case Some(next) => Some(PatternPoint(pat, elem.copy(point = next)))
      case None => Some(PatternPoint(pat, AfterList))
    }
    case AfterList => None
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

  def isPatternPoint: Boolean = point.bodies match {
    case BeforeList => true
    case _ => false
  }

  def isPatternEndPoint: Boolean = point.bodies match {
    case AfterList => true
    case _ => false
  }

  def isBodyPoint: Boolean = point.bodies match {
    case AtListElem(_, _, BodyPoint(_, BeforeList)) => true
    case _ => false
  }

  def isBodyEndPoint: Boolean = point.bodies match {
    case AtListElem(_, _, BodyPoint(_, AfterList)) => true
    case _ => false
  }

  def isAtomPoint: Boolean = point.bodies match {
    case AtListElem(_, _, BodyPoint(_, AtListElem(_, _, _))) => true
    case _ => false
  }

  def atom: Datalog.Atom =
    point.atom.getOrElse(throw new IllegalStateException(s"Cannot access atom of control point $this"))

//  def into(implicit patterns: Map[String, Datalog.Pattern]): Option[ControlPoint] = body match {
//    case Point.Before =>
//      Some(ControlPoint.bodyEntryPoint(pat, 0))
//    case Point.After =>
//      None
//    case Point.At(AtBody(bix, Point.Before)) =>
//      Some(ControlPoint.atomEntryPoint(pat, bix, 0))
//    case Point.At(AtBody(bix, Point.After)) =>
//      if (pat.bodies.size <= bix + 1)
//        Some(ControlPoint.patternEndPoint(pat))
//      else
//        Some(ControlPoint.bodyEntryPoint(pat, bix + 1))
//    case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
//      val atom = pat.bodies(bix).atoms(aix)
//      atom match {
//        case Datalog.Call(name, _, _, _) =>
//          Some(ControlPoint.patternEntryPoint(patterns(name)))
//        case _ =>
//          Some(ControlPoint.atomEndPoint(pat, bix, aix))
//      }
//    case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
//      if (pat.bodies(bix).atoms.size <= aix + 1)
//        Some(ControlPoint.bodyEndPoint(pat, bix))
//      else
//        Some(ControlPoint.atomEntryPoint(pat, bix, aix + 1))
//  }
//
//  def over: Option[ControlPoint] = body match {
//    case Point.Before =>
//      Some(ControlPoint.patternEndPoint(pat))
//    case Point.After =>
//      None
//    case Point.At(AtBody(bix, Point.Before)) =>
//      Some(ControlPoint.bodyEndPoint(pat, bix))
//    case Point.At(AtBody(bix, Point.After)) =>
//      Some(ControlPoint.patternEndPoint(pat))
//    case Point.At(AtBody(bix, Point.At(AtAtom(aix, true)))) =>
//      Some(ControlPoint.atomEndPoint(pat, bix, aix))
//    case Point.At(AtBody(bix, Point.At(AtAtom(aix, false)))) =>
//      if (pat.bodies(bix).atoms.size <= aix + 1)
//        Some(ControlPoint.bodyEndPoint(pat, bix))
//      else
//        Some(ControlPoint.atomEntryPoint(pat, bix, aix + 1))
//  }
//
//  def out: Option[ControlPoint] = body match {
//    case Point.Before =>
//      Some(ControlPoint.patternEndPoint(pat))
//    case Point.After =>
//      None
//    case Point.At(AtBody(bix, Point.Before)) =>
//      Some(ControlPoint.bodyEndPoint(pat, bix))
//    case Point.At(AtBody(bix, Point.After)) =>
//      Some(ControlPoint.patternEndPoint(pat))
//    case Point.At(AtBody(bix, Point.At(_))) =>
//      Some(ControlPoint.bodyEndPoint(pat, bix))
//  }
}
