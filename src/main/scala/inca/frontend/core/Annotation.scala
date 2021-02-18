package inca.frontend.core

import scala.collection.mutable

trait Annotations {
  private val annotations: mutable.Map[Annotation.Key, Annotation] = mutable.Map()
  def addAnnotation(anno: Annotation): this.type = {
    annotations += anno.key -> anno
    this
  }
  def withAnnotations(a: Annotations): this.type = {
    this.annotations.clear()
    this.annotations ++= a.annotations
    this
  }
  def hasAnnotation(key: Annotation.Key): Boolean =
    this.annotations.contains(key)
  def getAnnotation(key: Annotation.Key): Option[Annotation] =
    this.annotations.get(key)
}

trait Annotation {
  def key: Annotation.Key
}
object Annotation {
  type Key = String
}

object MainFunctionAnno extends Annotation {
  override def key: Annotation.Key = "MAIN_FUNCTION"
}