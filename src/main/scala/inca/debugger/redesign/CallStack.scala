package inca.debugger.redesign

import inca.util.Derivative

class CallStack {
  private var _stack: List[EvaluationPoint] = List()
  private var _observers: List[CallStack => Unit] = List()

  def frames: List[EvaluationPoint] = _stack

  def top: EvaluationPoint = _stack.head

  def get(index: Int): Option[EvaluationPoint] =
    _stack.lift(index)

  def isFinished: Boolean = _stack.isEmpty // _stack.size == 1 && top.cp.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty
  def size: Int = _stack.size

  def push(cp: EvaluationPoint): Unit = {
    _stack = cp :: _stack
    notifyStackChanged()
  }

  def pop(): EvaluationPoint = {
    val hd = _stack.head
    _stack = _stack.tail
    notifyStackChanged()
    hd
  }

  def clear(): Unit = {
    _stack = List()
    _observers = List()
  }

  def update(cp: EvaluationPoint): Unit = {
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
    _stack.map(_.pred).mkString("[", ", ", "]")
}
