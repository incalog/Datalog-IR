package inca.ir.typing


import scala.collection.mutable.ListBuffer
import inca.ir.util.SourceLocation
import inca.util.CompilationMessage

case class TypeErrorException(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

/* The Typechecker results */
trait TypeIO:
  private val errors: ListBuffer[CompilationMessage] = ListBuffer()
  private val warnings: ListBuffer[CompilationMessage] = ListBuffer()

  def error(msg: String, sourceLocations: SourceLocation*): Unit =
    errors += CompilationMessage(msg, sourceLocations, CompilationMessage.ERROR)
  def warn(msg: String, sourceLocations: SourceLocation*): Unit =
    warnings += CompilationMessage(msg, sourceLocations, CompilationMessage.WARNING)

  def getErrors: List[CompilationMessage] = errors.toList.distinct
  def getWarnings: List[CompilationMessage] = warnings.toList.distinct

  def hasTypeErrors: Boolean = errors.nonEmpty
  def hasTypeWarnings: Boolean = warnings.nonEmpty
  def printTypeIO(): Unit = {
    errors.foreach(println)
    warnings.foreach(println)
  }

  def failOnWarnings(): Unit = {
    val warnings = getWarnings
    if (warnings.nonEmpty)
      throw TypeErrorException(warnings)
  }
  
  def failOnError(): Unit = {
    val errors = getErrors
    if (errors.nonEmpty)
      throw TypeErrorException(errors)
  }

  def withErrors[A](f: => A): (A, List[CompilationMessage]) =
    val oldErrors = errors.toList
    errors.clear()
    val a = f
    val newErrors = errors.toList
    errors.clear()
    errors ++= oldErrors
    (a, newErrors)



object TypeIO:
  /* Errors that can occur in typechecking */
  case class TypeWarning(msg: String, sourceLocations: Seq[SourceLocation])
  case class TypeError(msg: String, sourceLocations: Seq[SourceLocation])