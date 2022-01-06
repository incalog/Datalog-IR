package inca.debugger


sealed trait DebugException {
  self: Throwable =>
  val message: String
}

case class IllegalDebugStateException(message: String)
  extends Exception(message) with DebugException

case class NoFunctionCallsException(message: String)
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
