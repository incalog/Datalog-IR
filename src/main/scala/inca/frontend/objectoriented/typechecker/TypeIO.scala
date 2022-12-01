package inca.frontend.objectoriented.typechecker

import inca.compiler
import inca.compiler.{CompilationMessage, SourceLocation}

import scala.collection.mutable.ListBuffer

trait TypeIO {
  private val errors: ListBuffer[CompilationMessage] = ListBuffer()
  private val warnings: ListBuffer[CompilationMessage] = ListBuffer()

  protected def error(msg: String, sourceLocations: SourceLocation*): Unit =
    errors += compiler.CompilationMessage(msg, sourceLocations, CompilationMessage.ERROR)

  protected def warn(msg: String, sourceLocations: SourceLocation*): Unit =
    warnings += compiler.CompilationMessage(msg, sourceLocations, CompilationMessage.WARNING)

  def getErrors: List[CompilationMessage] = errors.toList

  def getWarnings: List[CompilationMessage] = warnings.toList

  def hasErrors: Boolean = errors.nonEmpty

  def hasWarnings: Boolean = warnings.nonEmpty

  def printTypeIO(): Unit = {
    errors.foreach(println)
    warnings.foreach(println)
  }
}
