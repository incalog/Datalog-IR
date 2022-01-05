package inca.debugger

import inca.backend.ir.Datalog

import scala.collection.mutable

sealed trait CallFrame
case class PatternFrame(pat: Datalog.Pattern, args: PartialTuple) extends CallFrame
case class BodyFrame(body: Datalog.Body) extends CallFrame
case class AtomFrame(atom: Datalog.Atom) extends CallFrame
case class BodyEndFrame(body: Datalog.Body) extends CallFrame
case class PatternEndFrame(pat: Datalog.Pattern) extends CallFrame

class CallStack {
  private val _stack: mutable.Stack[CallFrame] = mutable.Stack.empty

  def frame: CallFrame = _stack.top

  def get(index: Int): Option[CallFrame] =
    if (index >= _stack.size)
      None
    else
      Some(_stack.toSeq(index))

  def enclosingBody: Option[Datalog.Body] = {
    var idx = 0
    while (idx < _stack.size) {
      _stack.toSeq(idx) match {
        case BodyFrame(body) => return Some(body)
        case _ =>
          idx = idx + 1
      }
    }
    None
  }

  def enclosingPattern: Option[Datalog.Pattern] = {
    var idx = 0
    while (idx < _stack.size) {
      _stack.toSeq(idx) match {
        case PatternFrame(pat, _) => return Some(pat)
        case _ =>
          idx = idx + 1
      }
    }
    None
  }

  def isFinished: Boolean = _stack.size == 1 && frame.isInstanceOf[PatternEndFrame]
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty

  def push(frame: CallFrame): Unit = _stack.push(frame)

  def pop(): CallFrame = _stack.pop()

  override def toString: String =
    _stack.mkString("[", ", ", "]")
}
