package inca.debugger

import scala.collection.mutable

// case class Breakpoint(cp: ControlPattern, bindings: BindingPattern, constraints: Seq[Datalog.Atom])

case class Frame(cp: ControlPoint, arguments: Table, bodySubst: Table, patternSubst: Table)

class CallStack {
  private val _stack: mutable.Stack[Frame] = mutable.Stack.empty

  def top: Frame = _stack.top

  def get(index: Int): Option[Frame] =
    if (index >= _stack.size)
      None
    else
      Some(_stack.toSeq(index))

  def isFinished: Boolean = _stack.size == 1 && top.cp.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty

  def push(cp: Frame): Unit = _stack.push(cp)

  def pop(): Frame = _stack.pop()

  override def toString: String =
    _stack.mkString("[", ", ", "]")
}
