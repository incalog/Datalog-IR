package inca.frontend.objectoriented.core

import inca.frontend.objectoriented.core.Annotation.Key

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

trait Annotation {
  def key: Annotation.Key
}
object Annotation {
  type Key = String
}

object MainFunctionAnno extends Annotation {
  override def key: Annotation.Key = "MAIN_FUNCTION"

  override def toString: String = "@main"
}

object OverrideFunctionAnno extends Annotation {
  override def key: Annotation.Key = "OVERRIDE_FUNCTION"

  override def toString: String = "@override"
}