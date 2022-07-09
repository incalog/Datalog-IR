package inca.debugger.old

import inca.debugger.Value
import inca.debugger.table.ImmutableTable
import inca.util.Derivative

case class Frame(
    cp: ControlPoint,
    argsTable: ImmutableTable[Value],
    bodyTable: ImmutableTable[Value])

object Frame {
  type Tables = (ImmutableTable[Value], ImmutableTable[Value])
  def apply(cp: ControlPoint, frameTables: Tables): Frame =
    Frame(cp, frameTables._1, frameTables._2)
}

class CallStack {
  private var _stack: List[Frame] = List()
  private var _observers: List[CallStack => Unit] = List()

  def frames: List[Frame] = _stack

  def top: Frame = _stack.head

  def get(index: Int): Option[Frame] =
    _stack.lift(index)

  def isFinished: Boolean = _stack.isEmpty // _stack.size == 1 && top.cp.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty
  def size: Int = _stack.size

  def push(cp: Frame): Unit = {
    _stack = cp :: _stack
    notifyStackChanged()
  }

  def pop(): Frame = {
    val hd = _stack.head
    _stack = _stack.tail
    notifyStackChanged()
    hd
  }

  def update(cp: Frame): Unit = {
    _stack = cp :: _stack.tail
    notifyStackChanged()
  }

  def addDerivative[T](init: CallStack => T)(f: CallStack => T): Derivative[CallStack, T] = {
    val deriv = new Derivative[CallStack, T](init(this), f)
    addObserver(deriv)
    deriv
  }
  def addObserver(obs: CallStack => Unit): Unit =
    _observers +:= obs
  private def notifyStackChanged(): Unit =
    _observers.foreach(_(this))

  override def toString: String =
    _stack.map(_.cp.point.pat.name).mkString("[", ", ", "]")
}
