package inca.ir.typing


import inca.ir.Failed

import scala.collection.mutable.ListBuffer
import inca.ir.typing.CompilationMessage.Severity
import inca.ir.util.SourceLocation

case class CompilationMessage(msg: String, sourceLocations: Seq[SourceLocation], severity: Severity):
  override def toString: String =
    s"""$msg
       |  in ${sourceLocations.mkString("\n  in ")}
       |""".stripMargin

object CompilationMessage:
  sealed class Severity(val id: Int, override val toString: String)
  object INFO    extends Severity(0, "INFO")
  object WARNING extends Severity(1, "WARNING")
  object ERROR   extends Severity(2, "ERROR")

/* The Typechecker results */
trait TypeIO:
  private val errors: ListBuffer[CompilationMessage] = ListBuffer()
  private val warnings: ListBuffer[CompilationMessage] = ListBuffer()

  def error(msg: String, sourceLocations: SourceLocation*): Unit =
    errors += CompilationMessage(msg, sourceLocations, CompilationMessage.ERROR)
  def warn(msg: String, sourceLocations: SourceLocation*): Unit =
    warnings += CompilationMessage(msg, sourceLocations, CompilationMessage.WARNING)

  def getErrors: List[CompilationMessage] = errors.toList
  def getWarnings: List[CompilationMessage] = warnings.toList

  def hasTypeErrors: Boolean = errors.nonEmpty
  def hasTypeWarnings: Boolean = warnings.nonEmpty
  def printTypeIO(): Unit = {
    errors.foreach(println)
    warnings.foreach(println)
  }

  def failOnError(): Unit = {
    val errors = getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }


object TypeIO:
  /* Errors that can occur in typechecking */
  case class TypeWarning(msg: String, sourceLocations: Seq[SourceLocation])
  case class TypeError(msg: String, sourceLocations: Seq[SourceLocation])