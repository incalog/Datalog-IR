package inca.frontend.functional.core

import inca.frontend.functional.core.Annotation.Key

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

object InvariantAnno extends Annotation {
  override def key: Annotation.Key = "INVARIANT"
  override def toString: String = "@invariant"
}

case class UsesInvariantAnno(invariantNames: Seq[Name]) extends Annotation {
  override def key: Annotation.Key = "USES_INVARIANTS"
  override def toString: String = s"@uses(${invariantNames.mkString(", ")})"
}

case class AggregationAnno(props: Seq[AggregationProperty]) extends Annotation {
  override def key: Annotation.Key = "AGGREGATION"
  override def toString: String = s"@aggr(${props.mkString(", ")})"
}

trait AggregationProperty {
  def name: String
  def inverseName: Option[String]
  override def toString: Key = name
}
case object Associativity extends AggregationProperty {
  override def name: String = "assoc"
  override def inverseName: Option[String] = None
}
case object Commutativity extends AggregationProperty {
  override def name: String = "comm"
  override def inverseName: Option[String] = None
}
case class Invertibility(invName: String) extends AggregationProperty {
  override def name: String = s"invert($invName)"
  override def inverseName: Option[String] = Some(invName)
}