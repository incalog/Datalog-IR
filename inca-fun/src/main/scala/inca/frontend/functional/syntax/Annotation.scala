package inca.frontend.functional.syntax

import inca.frontend.functional.syntax.Annotation.Key
import inca.ir.util.SourceLocation

import scala.sys.process.ProcessBuilder.Source

trait Annotations {
  protected val annos: Seq[Annotation]
  protected lazy val annoMap: Map[Key, Annotation] = annos.map(a => a.key -> a).toMap

  def hasAnnotation(key: Annotation.Key): Boolean =
    this.annoMap.contains(key)

  def getAnnotation(key: Annotation.Key): Option[Annotation] =
    this.annoMap.get(key)

  def annoPrefix(implicit indent: String): String = if (annos.isEmpty) "" else indent + annoString + "\n"

  def annoString: String = annos.mkString(" ")
}

trait Annotation extends SourceLocation {
  def key: Annotation.Key
}

object Annotation {
  type Key = String
}

object MainFunctionAnno:
  val KEY: Annotation.Key = "MAIN_FUNCTION"

case class MainFunctionAnno() extends Annotation {
  override def key: Annotation.Key = MainFunctionAnno.KEY

  override def toString: String = "@main"
}