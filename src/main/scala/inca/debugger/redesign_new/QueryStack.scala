package inca.debugger.redesign_new

import inca.util.Derivative

class QueryStack {
  private var _stack: List[Query] = List()
  private var _observers: List[QueryStack => Unit] = List()

  def frames: List[Query] = _stack

  def top: Query = _stack.head

  def get(index: Int): Option[Query] =
    _stack.lift(index)

  def isFinished: Boolean = _stack.isEmpty // _stack.size == 1 && top.cp.isPatternEndPoint
  def isEmpty: Boolean = _stack.isEmpty
  def nonEmpty: Boolean = _stack.nonEmpty
  def size: Int = _stack.size

  def push(q: Query): Unit = {
    _stack = q :: _stack
    notifyStackChanged()
  }

  def pop(): Query = {
    val hd = _stack.head
    _stack = _stack.tail
    notifyStackChanged()
    hd
  }

  def clear(): Unit = {
    _stack = List()
    _observers = List()
  }

  def update(q: Query): Unit = {
    _stack = q :: _stack.tail
    notifyStackChanged()
  }

  def update(q: Query, idx: Int): Unit = {
    // _stack = q :: _stack.tail
    _stack = _stack.updated(size - idx, q)
    notifyStackChanged()
  }

  def addDerivative[T](init: QueryStack => T)(f: QueryStack => T): Derivative[QueryStack, T] = {
    val deriv = new Derivative[QueryStack, T](init(this), f)
    addObserver(deriv)
    deriv
  }
  def addObserver(obs: QueryStack => Unit): Unit =
    _observers +:= obs
  private def notifyStackChanged(): Unit =
    _observers.foreach(_(this))

  override def toString: String =
    _stack.map(_.predicate).mkString("[", ", ", "]")
}
