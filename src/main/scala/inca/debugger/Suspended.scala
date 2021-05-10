package inca.debugger

trait Suspended {
  val parent: Option[Suspended]
  def stepOver(env: Environment): (Suspended, Environment)

  def stepInto(env: Environment): (Suspended, Environment, CallFrame)
  def canStepInto(env: Environment): Boolean
}
