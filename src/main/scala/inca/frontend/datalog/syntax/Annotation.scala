package inca.frontend.datalog.syntax

import inca.frontend.functional.core.Annotation.Key

trait Annotations {
  protected val annos: Seq[Annotation]
  protected lazy val annoMap: Map[Key, Annotation] = annos.map(a => a.key -> a).toMap

  def hasAnnotation(key: Annotation.Key): Boolean =
    this.annoMap.contains(key)
  def getAnnotation(key: Annotation.Key): Option[Annotation] =
    this.annoMap.get(key)

  def annoPrefix(implicit indent: String): String = if (annos.isEmpty) "" else indent + annoString + " "
  def annoString: String = annos.mkString(" ")
}

trait Annotation {
  def key: Annotation.Key
}
object Annotation {
  type Key = String
}

object ExtensionalAnno extends Annotation {
  override def key: Annotation.Key = "EXTENSIONAL"
  override def toString: String = "@extensional"
}
object MainAnno extends Annotation {
  override def key: Annotation.Key = "MAIN"
  override def toString: String = "@main"
}
