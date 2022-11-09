package inca.frontend.runner

import inca.frontend.Constants.RelationName
import inca.runtime.Query

object Relation {
  def from(relName: RelationName, parameterNames: Seq[RelationName], matches: Iterable[Seq[Any]]): Relation =
    parameterNames.size match {
      case 0 => UnitRelation(relName)
      case 1 => Relation1(relName, parameterNames, matches)
      case 2 => Relation2(relName, parameterNames, matches)
      case 3 => Relation3(relName, parameterNames, matches)
    }

  protected[frontend] def fromQueryMatches(relName: RelationName, parameterNames: Seq[RelationName], matches: Iterable[Query.Match]): Relation =
    parameterNames.size match {
      case 0 => UnitRelation(relName)
      case 1 => Relation1(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 2 => Relation2(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 3 => Relation3(relName, parameterNames, matches.map(_.toArray.toSeq))
    }
}

trait Relation {
  type Tuple
  def name: RelationName
  def arity: Int
  def size: Int
  def parameterNames: Seq[String]
  def entries: Iterable[Tuple]
  def toSet: Set[Tuple] = entries.toSet
  def flattenEntry(entry: Tuple): Seq[AnyRef]

  if (parameterNames.size != arity)
    throw new IllegalArgumentException(s"Expected $arity parameter names but got ${parameterNames.size}.")

  private def rawEntries: Iterable[Seq[Any]] = entries.map {
    case p: Product => p.productIterator.toSeq
    case e if parameterNames.nonEmpty => Seq(e)
    case _ => Seq()
  }

  def slice(from: Int, until: Int): Relation = {
    val outputParamNames = parameterNames.slice(from, until)
    val outputValues = rawEntries.map(_.slice(from, until).toSeq)
    Relation.from(name, outputParamNames, outputValues)
  }

  override def toString: RelationName = {
    val entriesS = rawEntries.map { e =>
      parameterNames.zip(e).map { case (name, value) =>
        s"$name: $value"
      }.mkString("(", ", ", ")")
    }.mkString("{", ", ", "}")
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: ${entriesS})"
  }
}

case class UnitRelation(name: RelationName) extends Relation {
  type Tuple = Unit
  lazy val arity: Int = 0
  lazy val size: Int = 0
  lazy val parameterNames: Seq[String] = Seq.empty
  lazy val entries: Iterable[Tuple] = Iterable.empty
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq.empty
}

case class Relation1[A <: AnyRef](name: RelationName,
                                  parameterNames: Seq[String],
                                  tuples: Iterable[Seq[Any]]) extends Relation {
  type Tuple = A

  lazy val arity: Int = 1
  lazy val size: Int = tuples.size
  lazy val entries: Iterable[Tuple] = tuples.map(_.head.asInstanceOf[A])
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry)
}

case class Relation2[A1 <: AnyRef, A2 <: AnyRef](name: RelationName,
                                                 parameterNames: Seq[String],
                                                 matches: Iterable[Seq[Any]]) extends Relation {
  type Tuple = (A1, A2)

  lazy val arity: Int = 2
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { entry =>
    (entry.head.asInstanceOf[A1], entry(1).asInstanceOf[A2])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2)
}

case class Relation3[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef](name: RelationName,
                                                               parameterNames: Seq[String],
                                                               matches: Iterable[Seq[Any]]) extends Relation {
  type Tuple = (A1, A2, A3)

  lazy val arity: Int = 3
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { entry =>
    (entry.head.asInstanceOf[A1], entry(1).asInstanceOf[A2], entry(2).asInstanceOf[A3])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3)
}