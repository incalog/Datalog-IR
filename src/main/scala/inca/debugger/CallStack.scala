package inca.debugger

import inca.frontend.core.tree.Name

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[debugger] class CallStack {
  val stack: ListBuffer[StackFrame] = ListBuffer.empty

  def currentAddr: Int = stack.size - 1

  def currentFun: Name = frame().funName
  def currentBody: Int = frame().body
  def currentPntr: Int = frame().pntr

  def frame(addr: Int = currentAddr): StackFrame = {
    if(addr < 0 || addr >= stack.length)
      throw InvalidPointerException(s"Invalid frame address $addr in stack of ${stack.length} frames")

    stack(addr)
  }

  def push(frame: StackFrame): Unit = {
    stack += frame
  }

  def pop(): StackFrame = {
    stack.remove(currentAddr)
  }

  override def toString: String = "Graph(f:" + currentAddr + ")\n" + stack.toSeq.map(f => "::" + f.toString).mkString("\n")
}

private[debugger] class StackFrame(val caller: Int, val funName: Name, val args: Seq[Set[ColumnValue]], val body: Int) {
  val env: mutable.Map[Name, Set[ColumnValue]] = mutable.Map()
  val intermVars: mutable.Map[(Name, Seq[Set[ColumnValue]], Int), Set[ColumnValue]] = mutable.Map()
  var pntr: Int = -1

  def incrementPntr(): Unit = pntr += 1

  override def toString: String = s"Frame($caller->${funName.name}:$body) {${env.toString()}}"
}
