package inca.debugger

import inca.backend.ir.Datalog

import scala.collection.mutable

/**
 * Edge(x,y), z := y, IsConnected(x,y)
 *
 *
 * type Table = (Columns, ColumnsIndices, Data)
 * type ColumnsIndices = Map[String, Int]
 * type Data = Map[ArraySeq[V], Int]
 */

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
    case Point.At(t) => false
    case _ => true
  }

  def isPatternEndPoint: Boolean = body match {
    case Point.After => true
    case _ => false
  }

  def isBodyPoint: Boolean = body match {
    case Point.At(t) => t.atom match {
      case Point.At(t) => false
      case _ => true
    }
    case _ => false
  }

  def isAtomPoint: Boolean = body match {
    case Point.At(t) => t.atom match {
      case Point.At(t) => true
      case _ => false
    }
    case _ => false
  }
}

case class Frame(cp: ControlPoint, bindings: Any, priorResults: Any)

// ccase class Breakpoint(cp: ControlPattern, bindings: BindingPattern, constraints: Seq[Datalog.Atom])


class CallStack {
  private val _stack: mutable.Stack[ControlPoint] = mutable.Stack.empty

  def top: ControlPoint = _stack.top

  def get(index: Int): Option[ControlPoint] =
    if (index >= _stack.size)
      None
    else
      Some(_stack.toSeq(index))

  def isFinished: Boolean = _stack.size == 1 && top.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty

  def push(cp: ControlPoint): Unit = _stack.push(cp)

  def pop(): ControlPoint = _stack.pop()

  override def toString: String =
    _stack.mkString("[", ", ", "]")
}
