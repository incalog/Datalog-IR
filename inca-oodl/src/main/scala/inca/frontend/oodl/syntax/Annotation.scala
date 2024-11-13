package inca.frontend.oodl.syntax

import inca.ir.util.SourceLocation

import Annotation.Key

trait Annotations {
  protected val annos: Seq[Annotation]
  protected lazy val annoMap: Map[Key, Annotation] = annos.map(a => a.key -> a).toMap

  def hasAnnotation(key: Annotation.Key): Boolean =
    this.annoMap.contains(key)

  def getAnnotation(key: Annotation.Key): Option[Annotation] =
    this.annoMap.get(key)

  def annoPrefix(implicit indent: String): String = if (annos.isEmpty) "" else annoString + " "

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

object OverrideFunctionAnno:
  val KEY: Annotation.Key = "OVERRIDE_FUNCTION"

case class OverrideFunctionAnno() extends Annotation {
  override def key: Annotation.Key = OverrideFunctionAnno.KEY

  override def toString: String = "override"
}

object GeneratedConstructorFieldAnno:
  val KEY: Annotation.Key = "GENERATED_CONSTRUCTOR_FIELD"

case class GeneratedConstructorFieldAnno() extends Annotation {
  override def key: Annotation.Key = GeneratedConstructorFieldAnno.KEY

  override def toString: String = "@generated"
}

object CaseClassAnno:
  val KEY: Annotation.Key = "CASE_CLASS"

case class CaseClassAnno() extends Annotation {
  override def key: Annotation.Key = CaseClassAnno.KEY

  override def toString: String = "case"
}

object MonoClassAnno:
  val KEY: Annotation.Key = "MONO_CLASS"

case class MonoClassAnno() extends Annotation {
  override def key: Annotation.Key = MonoClassAnno.KEY

  override def toString: String = "mono"
}