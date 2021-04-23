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

  def incrementPntr(): Unit = ptr += 1

  override def toString: String = {
    val vars = env.map {
      case (name, evs) => (name, evs.map(ev => ev.columnValue).mkString("{", ", ", "}"))
    }
    vars.mkString(s"Frame::${funName.name} [ ", ", ", " ]")
  }

  def prettyPrint(): Unit = {
    val allPs: Set[Name] = env.flatMap {
      case (_, evs) => evs.flatMap(ev => ev.parents.map(p => p._1))
    }.toSet

    val leaves = env.keys.toSet -- allPs
    val res = leaves.map(name => {
        env(name).map(ev => getRelatedColumns(name, ev))
    }).foldLeft(Set[Seq[(Name, ColumnValue)]]()) {
      case (z, s) => if(z.isEmpty) s else combine(z, s)
    }

    val pOut = res.map(tup => {
      tup.map(column => s"${column._1}=${column._2}").mkString("{", ", ", "}")
    }).mkString("\n")

    println(s"Frame::${funName.name}\n$pOut")
  }

  def getRelatedColumns(name: Name, value: EnvValue): Seq[(Name, ColumnValue)] = {
    val parents = value.parents.flatMap {
      case (n, cv) => env.getOrElse(n, Set())
        .filter(ev => ev.columnValue == cv && (n, ev) != (name, value))
        .map(ev => (n, ev))
    }
    parents.map {
      case (n, ev) => getRelatedColumns(n, ev)
    }.foldRight(Seq[(Name, ColumnValue)]((name, value.columnValue))) {
      (z, s) => z ++ s
    }
  }

  private def combine(as: Set[Seq[(Name, ColumnValue)]], bs: Set[Seq[(Name, ColumnValue)]]): Set[Seq[(Name, ColumnValue)]] = {
    bs.flatMap(b => as.map(a => b ++ a))
  }
}

private[debugger] class ContainerFrame(val parent: Int, val funName: Name, val args: Seq[Set[EnvValue]]) extends Frame {
  val params: mutable.Map[Name, Set[EnvValue]] = mutable.Map()
}
