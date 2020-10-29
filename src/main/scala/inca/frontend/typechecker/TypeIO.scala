package inca.frontend.typechecker

import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.TypeIO._

import scala.collection.mutable.ListBuffer


/* The Typechecker results */
trait TypeIO {
  private val errors: ListBuffer[TypeError] = ListBuffer()
  private val warnings: ListBuffer[TypeWarning] = ListBuffer()

  protected def error(msg: String, sourceLocations: SourceLocation*): Unit =
    errors += TypeError(msg, sourceLocations)
  protected def warn(msg: String, sourceLocations: SourceLocation*): Unit =
    warnings += TypeWarning(msg, sourceLocations)

  def getErrors: List[TypeError] = errors.toList
  def getWarnings: List[TypeWarning] = warnings.toList

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