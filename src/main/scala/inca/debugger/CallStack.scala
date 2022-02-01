package inca.debugger

import inca.debugger.table.Table

import scala.collection.mutable

// case class Breakpoint(cp: ControlPattern, bindings: BindingPattern, constraints: Seq[Datalog.Atom])

case class Frame(cp: ControlPoint, argsTable: Table[Value], bodyTable: Table[Value])
object Frame {
  type Tables = (Table[Value], Table[Value])
  def apply(cp: ControlPoint, frameTables: Tables): Frame =
    Frame(cp, frameTables._1, frameTables._2)
}

class CallStack {
  private var _stack: List[Frame] = List()

  def frames: List[Frame] = _stack

  def top: Frame = _stack.head

  def get(index: Int): Option[Frame] =
    _stack.lift(index)

  def isFinished: Boolean = _stack.isEmpty // _stack.size == 1 && top.cp.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty
  def size: Int = _stack.size

  def push(cp: Frame): Unit = _stack = cp :: _stack

  def pop(): Frame = {
    val hd = _stack.head
    _stack = _stack.tail
    hd
  }

  def update(cp: Frame): Unit =
    _stack = cp :: _stack.tail

  override def toString: String =
    _stack.map(_.cp.point.pat.name).mkString("[", ", ", "]")
}
