package inca.ir.execution

import inca.ir.execution
import inca.util.Tabulator

type RelationName = String

object Relation {
  def from(relName: RelationName, matches: Seq[Any]): Relation =
    val params = matches.indices.map(i => s"param_$i")
    Relation.from(relName, params, Seq(matches))

  def from(relName: RelationName, paramNames: Seq[String], queryMatches: Iterable[Seq[Any]]): Relation =
    paramNames.size match {
      case 0 => UnitRelation(relName)
      case 1 => Relation1(relName, paramNames, queryMatches)
      case 2 => Relation2(relName, paramNames, queryMatches)
      case 3 => Relation3(relName, paramNames, queryMatches)
      case 4 => Relation4(relName, paramNames, queryMatches)
      case 5 => Relation5(relName, paramNames, queryMatches)
      case 6 => Relation6(relName, paramNames, queryMatches)
      case 7 => Relation7(relName, paramNames, queryMatches)
      case 8 => Relation8(relName, paramNames, queryMatches)
      case 9 => Relation9(relName, paramNames, queryMatches)
      case 10 => Relation10(relName, paramNames, queryMatches)
      case 11 => Relation11(relName, paramNames, queryMatches)
      case 12 => Relation12(relName, paramNames, queryMatches)
      case 13 => Relation13(relName, paramNames, queryMatches)
      case 14 => Relation14(relName, paramNames, queryMatches)
      case 15 => Relation15(relName, paramNames, queryMatches)
      case 16 => Relation16(relName, paramNames, queryMatches)
      case 17 => Relation17(relName, paramNames, queryMatches)
      case 18 => Relation18(relName, paramNames, queryMatches)
      case 19 => Relation19(relName, paramNames, queryMatches)
      case 20 => Relation20(relName, paramNames, queryMatches)
      case 21 => Relation21(relName, paramNames, queryMatches)
      case 22 => Relation22(relName, paramNames, queryMatches)
      case 23 => Relation23(relName, paramNames, queryMatches)
      case 24 => Relation24(relName, paramNames, queryMatches)
      case 25 => Relation25(relName, paramNames, queryMatches)
      case _ =>
        // If we have too many parameters, we just fallback to a generic relation instance
        new Relation:
          override type Tuple = Seq[Any]

          override def name: RelationName = relName

          override def arity: Int = paramNames.size

          override def size: Int = queryMatches.size

          override def parameterNames: Seq[_root_.java.lang.String] = paramNames

          override def entries: Iterable[Seq[Any]] = queryMatches

          override def unflattenEntry(entry: Seq[Any]): Seq[Any] = entry

          override def matches: Iterable[Seq[Any]] = queryMatches
    }
}

trait Relation {
  type Tuple

  def name: RelationName

  def arity: Int

  def size: Int

  def nonEmpty: Boolean = size > 0

  def isEmpty: Boolean = size == 0

  def parameterNames: Seq[String]

  def entries: Iterable[Tuple]

  def toSet: Set[Tuple] = entries.toSet

  def flattenEntry(entry: Tuple): Seq[AnyRef] =
    if (arity == 0)
      Seq()
    else if (arity == 1)
      Seq(entry.asInstanceOf[AnyRef])
    else entry match
      case v: Product => v.productIterator.map(_.asInstanceOf[AnyRef]).toSeq
      case s: Iterable[_] => s.map(_.asInstanceOf[AnyRef]).toSeq

  def unflattenEntry(entry: Seq[Any]): Tuple

  def matches: Iterable[Seq[Any]]

  def contains(tup: Tuple): Boolean =
    entries.exists(_ == tup)

  if (parameterNames.size != arity)
    throw new IllegalArgumentException(s"Expected $arity parameter names but got ${parameterNames.size}.")

  def project(from: Int, until: Int = Int.MaxValue): Relation = {
    val outputParamNames = parameterNames.slice(from, until)
    val outputValues = matches.map(_.slice(from, until).toSeq)
    Relation.from(name, outputParamNames, outputValues)
  }

  override def equals(obj: Any): Boolean = obj match {
    case relation: Relation =>
      val sameName = name == relation.name
      val sameArity = arity == relation.arity
      val sameSize = size == relation.size
      // TODO: Compare ADT data correctly. For now we just compare the string rep, which might fail when we use different engines
      val sameElements = toSet.map(t => flattenEntry(t).toString) == relation.toSet.map(t => relation.flattenEntry(t).toString)
      sameName && sameArity && sameSize && sameElements
    case _ => false
  }

  override def toString: RelationName = {
    val entriesS = matches.map { e =>
      parameterNames.zip(e).map { case (name, value) =>
        s"$name: $value"
      }.mkString("(", ", ", ")")
    }.mkString("{", ", ", "}")
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: $entriesS)"
  }

  def asTable: String = Tabulator.format(s"$name - $size", parameterNames, matches.toSeq)
}

case class UnitRelation(name: RelationName) extends Relation {
  case class Tuple()

  lazy val arity: Int = 0
  lazy val size: Int = 0
  lazy val parameterNames: Seq[String] = Seq.empty
  lazy val entries: Iterable[Tuple] = Iterable.empty

  override def unflattenEntry(entry: Seq[Any]): Tuple = Tuple()

  lazy val matches: Iterable[Seq[Any]] = Seq.empty
}

case class Relation1[A <: AnyRef](name: RelationName,
                                  parameterNames: Seq[String],
                                  matches: Iterable[Seq[Any]]
                                 ) extends Relation {
  type Tuple = A

  lazy val arity: Int = 1
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(_.head.asInstanceOf[A])

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry.head.asInstanceOf[A]
}


case class Relation2[
  A1 <: AnyRef,
  A2 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2)

  lazy val arity: Int = 2
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2])
}


case class Relation3[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3)

  lazy val arity: Int = 3
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3])
}


case class Relation4[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4)

  lazy val arity: Int = 4
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4])
}


case class Relation5[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5)

  lazy val arity: Int = 5
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5])
}


case class Relation6[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6)

  lazy val arity: Int = 6
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6])
}


case class Relation7[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7)

  lazy val arity: Int = 7
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7) => (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7])
}


case class Relation8[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8)

  lazy val arity: Int = 8
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8])
}


case class Relation9[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9)

  lazy val arity: Int = 9
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9])
}


case class Relation10[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)

  lazy val arity: Int = 10
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10])
}


case class Relation11[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)

  lazy val arity: Int = 11
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11])
}


case class Relation12[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)

  lazy val arity: Int = 12
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12])
}


case class Relation13[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)

  lazy val arity: Int = 13
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13])
}


case class Relation14[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)

  lazy val arity: Int = 14
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14])
}


case class Relation15[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)

  lazy val arity: Int = 15
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15])
}


case class Relation16[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)

  lazy val arity: Int = 16
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16])
}


case class Relation17[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)

  lazy val arity: Int = 17
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17])
}


case class Relation18[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)

  lazy val arity: Int = 18
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18])
}


case class Relation19[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)

  lazy val arity: Int = 19
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19])
}


case class Relation20[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)

  lazy val arity: Int = 20
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20])
}

case class Relation21[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef,
  A21 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)

  lazy val arity: Int = 21
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match {
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20], a21.asInstanceOf[A21])
  }
}

case class Relation22[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef,
  A21 <: AnyRef,
  A22 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)

  lazy val arity: Int = 22
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match {
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20], a21.asInstanceOf[A21], a22.asInstanceOf[A22])
  }
}

case class Relation23[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef,
  A21 <: AnyRef,
  A22 <: AnyRef,
  A23 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23)

  lazy val arity: Int = 23
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match {
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22, a23) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20],
        a21.asInstanceOf[A21], a22.asInstanceOf[A22], a23.asInstanceOf[A23])
  }
}

case class Relation24[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef,
  A21 <: AnyRef,
  A22 <: AnyRef,
  A23 <: AnyRef,
  A24 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24)

  lazy val arity: Int = 24
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match {
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22, a23, a24) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20],
        a21.asInstanceOf[A21], a22.asInstanceOf[A22], a23.asInstanceOf[A23], a24.asInstanceOf[A24])
  }
}

case class Relation25[
  A1 <: AnyRef,
  A2 <: AnyRef,
  A3 <: AnyRef,
  A4 <: AnyRef,
  A5 <: AnyRef,
  A6 <: AnyRef,
  A7 <: AnyRef,
  A8 <: AnyRef,
  A9 <: AnyRef,
  A10 <: AnyRef,
  A11 <: AnyRef,
  A12 <: AnyRef,
  A13 <: AnyRef,
  A14 <: AnyRef,
  A15 <: AnyRef,
  A16 <: AnyRef,
  A17 <: AnyRef,
  A18 <: AnyRef,
  A19 <: AnyRef,
  A20 <: AnyRef,
  A21 <: AnyRef,
  A22 <: AnyRef,
  A23 <: AnyRef,
  A24 <: AnyRef,
  A25 <: AnyRef](name: RelationName, parameterNames: Seq[String], matches: Iterable[Seq[Any]]) extends Relation {

  type Tuple = (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, A23, A24, A25)

  lazy val arity: Int = 25
  lazy val size: Int = matches.size
  lazy val entries: Iterable[Tuple] = matches.map(unflattenEntry)

  override def unflattenEntry(entry: Seq[Any]): Tuple = entry match {
    case Seq(a1, a2, a3, a4, a5, a6, a7,
    a8, a9, a10, a11, a12, a13, a14,
    a15, a16, a17, a18, a19, a20, a21, a22, a23, a24, a25) =>
      (a1.asInstanceOf[A1], a2.asInstanceOf[A2], a3.asInstanceOf[A3], a4.asInstanceOf[A4], a5.asInstanceOf[A5], a6.asInstanceOf[A6], a7.asInstanceOf[A7],
        a8.asInstanceOf[A8], a9.asInstanceOf[A9], a10.asInstanceOf[A10], a11.asInstanceOf[A11], a12.asInstanceOf[A12], a13.asInstanceOf[A13], a14.asInstanceOf[A14],
        a15.asInstanceOf[A15], a16.asInstanceOf[A16], a17.asInstanceOf[A17], a18.asInstanceOf[A18], a19.asInstanceOf[A19], a20.asInstanceOf[A20],
        a21.asInstanceOf[A21], a22.asInstanceOf[A22], a23.asInstanceOf[A23], a24.asInstanceOf[A24], a25.asInstanceOf[A25])
  }
}