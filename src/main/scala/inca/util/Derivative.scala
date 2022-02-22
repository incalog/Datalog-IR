package inca.util

class Derivative[I, T](init: T, f: I => T) extends (I => Unit) {
  private var _value: T = init
  def value: T = _value
  def apply(v: I): Unit = 
    _value = f(v)
}
