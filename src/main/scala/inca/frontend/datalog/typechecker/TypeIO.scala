package inca.frontend.datalog.typechecker

import inca.compiler
import inca.compiler.source.SourceLocation
import inca.compiler.CompilationMessage
import scala.collection.mutable.ListBuffer

/* The Typechecker results */
trait TypeIO {
  private val errors: ListBuffer[CompilationMessage] = ListBuffer()
  private val warnings: ListBuffer[CompilationMessage] = ListBuffer()

  protected def error(msg: String, loc: SourceLocation, sourceLocations: SourceLocation*): Unit =
    errors += compiler.CompilationMessage(msg, loc +: sourceLocations, CompilationMessage.ERROR)
  protected def warn(msg: String, loc: SourceLocation, sourceLocations: SourceLocation*): Unit =
    warnings += compiler.CompilationMessage(msg, loc +: sourceLocations, CompilationMessage.WARNING)

  def getErrors: List[CompilationMessage] = errors.toList
  def getWarnings: List[CompilationMessage] = warnings.toList

  def hasTypeErrors: Boolean = errors.nonEmpty
  def hasTypeWarnings: Boolean = warnings.nonEmpty
  def printTypeIO(): Unit = {
    errors.foreach(println)
    warnings.foreach(println)
  }
}

object TypeIO {
  /* Errors that can occur in typechecking */
  case class TypeWarning(msg: String, sourceLocations: Seq[SourceLocation])
  case class TypeError(msg: String, sourceLocations: Seq[SourceLocation])
}
