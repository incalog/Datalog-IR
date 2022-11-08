package inca.frontend.runner

import inca.runtime.Query

object Relation {
  def from(parameterNames: Seq[String], relName: RelationName, matches: Iterable[Query.Match]): Relation =
    parameterNames.size match {
      case 0 => UnitRelation(relName)
      case 1 => Relation1(relName, parameterNames.head, matches)
      case 2 => Relation2(relName, parameterNames, matches)
    }
}

trait Relation {
  type Tuple
  def name: RelationName
  def size: Int
  def parameterNames: Seq[String]
  def entries: Iterable[Tuple]
  def toSet: Set[Tuple] = entries.toSet
  def flattenEntry(entry: Tuple): Seq[AnyRef]

  if (parameterNames.size != size)
    throw new IllegalArgumentException(s"Expected $size parameter names but got ${parameterNames.size}.")

  override def toString: RelationName = {
    val namedEntries = entries.map {
      case p: Product => parameterNames.zip(p.productIterator)
      case v if parameterNames.nonEmpty => Seq(parameterNames.head -> v)
      case _ => Seq()
    }
    val entriesS = namedEntries.map { e =>
      e.map {
        case (name, value) => s"$name: $value"
      }.mkString("(", ", ", ")")
    }.toSet
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: ${entriesS}}"
  }
}

case class UnitRelation(name: RelationName) extends Relation {
  type Tuple = Unit
  lazy val size: Int = 0
  lazy val parameterNames: Seq[String] = Seq.empty
  lazy val entries: Iterable[Tuple] = Iterable.empty
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq.empty
}

case class Relation1[A <: AnyRef](name: RelationName,
                                  parameterName: String,
                                  matches: Iterable[Query.Match]) extends Relation {
  type Tuple = A
  lazy val size: Int = matches.size
  lazy val parameterNames: Seq[String] = Seq(parameterName)
  lazy val entries: Iterable[Tuple] = matches.map(_.toArray.head.asInstanceOf[A])
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry)
}

case class Relation2[A1 <: AnyRef, A2 <: AnyRef](name: RelationName,
                                                 parameterNames: Seq[String],
                                                 matches: Iterable[Query.Match]) extends Relation {
  type Tuple = (A1, A2)
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { m =>
    val entry = m.toArray
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2)
}

case class Relation3[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef](name: RelationName,
                                                               parameterNames: Seq[String],
                                                               matches: Iterable[Query.Match]) extends Relation {
  type Tuple = (A1, A2, A3)
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { m =>
    val entry = m.toArray
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2], entry(2).asInstanceOf[A3])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3)
}