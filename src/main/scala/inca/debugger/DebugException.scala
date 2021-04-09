package inca.debugger

import inca.frontend.core.tree.{Expression, Name}

sealed trait DebugException {
  self: Throwable =>
  val message: String
}


case class NoFunctionCallsException(message: String)
  extends Exception(message) with DebugException

case class MultipleFunctionCallsException(message: String,
                                          funs: Seq[(Name, Seq[Expression])],
                                          resolvedFuns: Seq[(Name, Seq[Set[EnvValue]])])
  extends Exception(message) with DebugException

case class MultipleBodiesException(message: String)
  extends Exception(message) with DebugException

case class EndOfTraversalReachedException(message: String)
  extends Exception(message) with DebugException

case class InvalidCommandException(message: String)
  extends Exception(message) with DebugException

case class InvalidPointerException(message: String)
  extends Exception(message) with DebugException

case class UnsupportedException(message: String)
  extends Exception(message) with DebugException

case class UnexpectedVarException(message: String)
  extends Exception(message) with DebugException
