package inca.debugger

import inca.frontend.core.tree.Name

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

private[debugger] class CallStack {
  val stack: ListBuffer[Frame] = ListBuffer.empty

  def currentAddr: Int = stack.size - 1
  def currentFun: Name = frame.funName
  def currentBody: Int = stackFrame.bodyPtr
  def currentPtr: Int = stackFrame.ptr

  def stackFrame: StackFrame = frame(currentAddr) match {
    case frame: StackFrame => frame
    case _: ContainerFrame => throw InvalidPointerException("ContainerFrame found, StackFrame expected")
  }

  def frame(addr: Int): Frame = {
    if(addr < 0 || addr >= stack.length)
      throw InvalidPointerException(s"Invalid frame address $addr in stack of ${stack.length} frames")

    stack(addr)
  }

  def frame: Frame = frame(currentAddr)

  def nonEmpty: Boolean = stack.nonEmpty
  def isEmpty: Boolean = stack.isEmpty

  def isStackFrame(addr: Int = currentAddr): Boolean = frame(addr).isInstanceOf[StackFrame]

  def push(frame: Frame): Unit =
    stack += frame

  def pop(): Frame =
    stack.remove(currentAddr)

  override def toString: String =
    stack.toSeq.filter(f => f.isInstanceOf[StackFrame]).reverse.mkString("Stack [\n  ", "\n  ", "\n]")
}

private[debugger] sealed trait Frame {
  def parent: Int
  def funName: Name
  def args: Seq[Set[ColumnValue]]
}

private[debugger] class StackFrame(val parent: Int, val funName: Name, val args: Seq[Set[ColumnValue]], val bodyPtr: Int = 0) extends Frame {
  val env: mutable.Map[Name, Set[ColumnValue]] = mutable.Map()
  val intermVars: mutable.Map[(Name, Seq[Set[ColumnValue]], Int), Set[ColumnValue]] = mutable.Map()

  var ptr: Int = -1

  def incrementPntr(): Unit = ptr += 1

  override def toString: String =
    env.mkString(s"Frame::${funName.name} { ", ", ", " }")
}

private[debugger] class ContainerFrame(val parent: Int, val funName: Name, val args: Seq[Set[ColumnValue]]) extends Frame {
  val params: mutable.Map[Name, Set[ColumnValue]] = mutable.Map()
}
