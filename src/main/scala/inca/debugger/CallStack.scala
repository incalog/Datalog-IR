package inca.debugger


import inca.backend.ir.Datalog.Name

import scala.collection.mutable.ListBuffer

private[debugger] class CallStack {
  private val _stack: ListBuffer[CallFrame] = ListBuffer.empty
  def frame: CallFrame = _stack.head

  def nonEmpty: Boolean = _stack.nonEmpty
  def isEmpty: Boolean = _stack.isEmpty

  def push(frame: CallFrame): Unit = {
    _stack.insert(0, frame)
  }

  def pop(): CallFrame =
    _stack.remove(_stack.size - 1)

  override def toString: String =
    _stack.mkString("Stack [\n  ", "\n  ", "\n]")
}


private[debugger] class CallFrame(val funName: Name, val funArgs: Environment, initialSuspended: Suspended) {
  var currentEnv: Environment = funArgs
  var currentSuspended: Suspended = initialSuspended

  override def toString: String = s"Frame::${funName} [ $funArgs ]"
}
