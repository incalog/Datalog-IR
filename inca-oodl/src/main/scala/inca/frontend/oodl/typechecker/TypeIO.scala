package inca.frontend.oodl.typechecker

import inca.ir.util.SourceLocation
import inca.util.CompilationMessage

import scala.collection.mutable.ListBuffer


/* The Typechecker results */
trait TypeIO {
  private val errors: ListBuffer[CompilationMessage] = ListBuffer()
  private val warnings: ListBuffer[CompilationMessage] = ListBuffer()

  protected def error(msg: String, sourceLocations: SourceLocation*): Unit =
    errors += CompilationMessage(msg, sourceLocations, CompilationMessage.ERROR)
  protected def warn(msg: String, sourceLocations: SourceLocation*): Unit =
    warnings += CompilationMessage(msg, sourceLocations, CompilationMessage.WARNING)

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