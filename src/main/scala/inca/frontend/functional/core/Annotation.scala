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

object PartialOrderAnno extends Annotation with VerifiableProperty {
  override def key: Annotation.Key = "PARTIAL_ORDER"
  override def toString: String = "@partialOrder"
}

case class InvariantAnno(invariantNames: Seq[String]) extends Annotation {
  override def key: Annotation.Key = "INVARIANT"
  override def toString: String = s"@invariant(${invariantNames.mkString(", ")})"
}
trait VerifiableProperty
/*
The annotated function f_a: A1 x A1 => A2 is supposed to be an overapproximation of the
concrete function f_c: C1 x C1 => C2 with approximation functions (betas) b1: C1 => A1
and b2: C2 => A2 using the given partial order (po)
 */
case class SoundnessAnno(concreteName: String, paramBetaName: String, resBetaName: String, poName: String) extends Annotation with VerifiableProperty {
  override def key: Annotation.Key = "SOUNDNESS"
  override def toString: String = s"@sound($concreteName, $paramBetaName, $resBetaName, $poName)"
}


case class AggregationAnno(props: Seq[AggregationProperty]) extends Annotation {
  override def key: Annotation.Key = "AGGREGATION"
  override def toString: String = s"@aggr(${props.mkString(", ")})"
}

trait AggregationProperty extends VerifiableProperty {
  def name: String

  override def toString: Key = name
}

case object Associativity extends AggregationProperty {
  override def name: String = "assoc"
}
case object Commutativity extends AggregationProperty {
  override def name: String = "comm"
}
case class HasUnapply(unapplyName: String) extends AggregationProperty {
  override def name: String = s"unapply($unapplyName)"
}


