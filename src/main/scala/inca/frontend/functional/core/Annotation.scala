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

// @partialOrder
case class PartialOrderAnno(dataName: String) extends Annotation {
  override def key: Annotation.Key = "MAIN_FUNCTION"
  override def toString: String = "@main"
}

case class InvariantAnno(invariantNames: Seq[String]) extends Annotation {
  override def key: Annotation.Key = "INVARIANT"
  override def toString: String = s"@invariant(${invariantNames.mkString(", ")})"
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
case class HasUnapply(unapplyName: String) extends AggregationProperty {
  override def name: String = s"unapply($unapplyName)"
  override def inverseName: Option[String] = Some(unapplyName)
  // TODO Ist inverseName eine sinnvolle Methode? Ich verwende es gar nicht
}

case class ApproxBy(approxName: String, beta1Name: String, beta2Name: String, poName: String) extends AggregationProperty {
  override def name: String = s"approx($approxName, $beta1Name, $beta2Name, $poName)" //approx(join, intToSign, intToSign, leq)
  override def inverseName: Option[String] = None
}

// TODO André fragen ob man das so machen kann
case object ApproxBy {
  def makeApprox(funNames: Seq[String]): ApproxBy =
    if(funNames.length == 4) ApproxBy(funNames(0), funNames(1), funNames(2), funNames(3)) else
      throw new Exception("Need four Arguments for ApproxBy Annotation")
}