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

private[debugger] case class EnvValue(columnValue: ColumnValue, parents: Set[(Name, ColumnValue)]) {
  override def toString: String = columnValue.toString
}

private[debugger] sealed trait Frame {
  type TArgList = Seq[Set[EnvValue]]

  def parent: Int
  def funName: Name
  def args: TArgList
}

private[debugger] class StackFrame(val parent: Int, val funName: Name, val args: Seq[Set[EnvValue]], val bodyPtr: Int = 0) extends Frame {
  val env: mutable.Map[Name, Set[EnvValue]] = mutable.Map()
  val intermVars: mutable.Map[(Name, TArgList, Int), (Set[EnvValue], TArgList)] = mutable.Map()
  var ptr: Int = -1

  def incrementPntr(): Unit =ptr += 1

  override def toString: String = {
    val vars = env.map {
      case (name, evs) => (name, evs.map(ev => ev.columnValue))
    }
    vars.mkString(s"Frame::${funName.name} { ", ", ", " }")
  }
}

private[debugger] class ContainerFrame(val parent: Int, val funName: Name, val args: Seq[Set[EnvValue]]) extends Frame {
  val params: mutable.Map[Name, Set[EnvValue]] = mutable.Map()
}
