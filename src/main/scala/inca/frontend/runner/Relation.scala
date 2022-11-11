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
      case 4 => Relation4(relName, parameterNames, matches)
      case 5 => Relation5(relName, parameterNames, matches)
    }

  protected[frontend] def fromQueryMatches(relName: RelationName, parameterNames: Seq[RelationName], matches: Iterable[Query.Match]): Relation =
    parameterNames.size match {
      case 0 => UnitRelation(relName)
      case 1 => Relation1(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 2 => Relation2(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 3 => Relation3(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 4 => Relation4(relName, parameterNames, matches.map(_.toArray.toSeq))
      case 5 => Relation5(relName, parameterNames, matches.map(_.toArray.toSeq))
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
  def matches: Iterable[Seq[Any]]

  if (parameterNames.size != arity)
    throw new IllegalArgumentException(s"Expected $arity parameter names but got ${parameterNames.size}.")

  def slice(from: Int, until: Int): Relation = {
    val outputParamNames = parameterNames.slice(from, until)
    val outputValues = matches.map(_.slice(from, until).toSeq)
    Relation.from(name, outputParamNames, outputValues)
  }

  override def equals(obj: Any): Boolean = obj match {
    case relation: Relation => this.toSet == relation.toSet
    case _ => false
  }

  override def toString: RelationName = {
    val entriesS = matches.map { e =>
      parameterNames.zip(e).map { case (name, value) =>
        s"$name: $value"
      }.mkString("(", ", ", ")")
    }.mkString("{", ", ", "}")
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: ${entriesS})"
  }

  def diff(other: Relation): Option[Relation] = {
    if (this.arity != other.arity)
      throw new IllegalArgumentException("Can only diff relations with same arity.")
    val change = this.matches.map(_.toSet).toSet.diff(other.matches.map(_.toSet).toSet)
    if (change.isEmpty)
      None
    else
      Some(Relation.from(this.name, this.parameterNames, change.map(_.toSeq)))
  }
}

case class UnitRelation(name: RelationName) extends Relation {
  type Tuple = Unit
  lazy val arity: Int = 0
  lazy val size: Int = 0
  lazy val parameterNames: Seq[String] = Seq.empty
  lazy val entries: Iterable[Tuple] = Iterable.empty
  lazy val matches: Iterable[Seq[Any]] = Seq.empty
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq.empty
}

case class Relation1[A <: AnyRef](name: RelationName,
                                  parameterNames: Seq[String],
                                  matches: Iterable[Seq[Any]]) extends Relation {
  type Tuple = A

  lazy val arity: Int = 1
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(_.head.asInstanceOf[A])
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry)
}

case class Relation2[A1 <: AnyRef, A2 <: AnyRef](name: RelationName,
                                                 parameterNames: Seq[String],
                                                 matches: Iterable[Seq[Any]]) extends Relation {
  type Tuple = (A1, A2)

  lazy val arity: Int = 2
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { entry =>
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2])
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
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2], entry(2).asInstanceOf[A3])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3)
}

case class Relation4[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef, A4 <: AnyRef](name: RelationName,
                                                                             parameterNames: Seq[String],
                                                                             matches: Iterable[Seq[Any]])
  extends Relation {

  type Tuple = (A1, A2, A3, A4)

  lazy val arity: Int = 4
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { entry =>
    (entry(0).asInstanceOf[A1], entry(1).asInstanceOf[A2], entry(2).asInstanceOf[A3], entry(2).asInstanceOf[A4])
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3, entry._4)
}

case class Relation5[A1 <: AnyRef, A2 <: AnyRef, A3 <: AnyRef, A4 <: AnyRef, A5 <: AnyRef](name: RelationName,
                                                                                           parameterNames: Seq[String],
                                                                                           matches: Iterable[Seq[Any]])
  extends Relation {

  type Tuple = (A1, A2, A3, A4, A5)

  lazy val arity: Int = 5
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map { entry =>
    (
      entry(0).asInstanceOf[A1],
      entry(1).asInstanceOf[A2],
      entry(2).asInstanceOf[A3],
      entry(2).asInstanceOf[A4],
      entry(2).asInstanceOf[A5]
    )
  }
  def flattenEntry(entry: Tuple): Seq[AnyRef] = Seq(entry._1, entry._2, entry._3, entry._4, entry._5)
}