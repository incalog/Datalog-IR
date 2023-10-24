package inca.foreign.scala.syntax

// TODO: Complete this
object Scala:
  trait Tree

  trait Type extends Tree
  trait Stat extends Tree
  trait Template extends Tree
  trait Defn extends Stat
  trait Mod extends Tree
  trait Term extends Stat

  case class TypeName(s: String) extends Type:
    override def toString: String = s

  case class FunType(args: Seq[Type], ret: Type) extends Type:
    override def toString: String = s"(${args.mkString(", ")}) => $ret"

  //case class TupleType(tys: Seq[Type]) extends Type:
  //  override def toString: String = s"(${tys.mkString(", ")})"

  // Mods
  case object Case extends Mod

  // Defn

  // TODO: complete these
  case class Object(name: String, mods: Seq[Mod] = Seq(), extending: Seq[String] = Seq()) extends Defn
  case class Trait(name: String, mods: Seq[Mod] = Seq(), extending: Seq[String] = Seq(), params: Seq[Param] = Seq()) extends Defn
  case class Class(name: String, mods: Seq[Mod] = Seq(), extending: Seq[String] = Seq(), params: Seq[Param] = Seq()) extends Defn


  // Fixme: This makes problem with the java class loader hack. Use traits instead
  case class EnumCase(name: String, values: Seq[Param]) extends Defn
  case class Enum(name: String, cases: Seq[EnumCase]) extends Defn

  // Terms

  case class Block(stats: Seq[Stat]) extends Term

  case class Var(x: String) extends Term:
    override def toString: String = x

  trait Literal[T] extends Term:
    val value: T
    override def toString: String = value.toString

  case class BoolLiteral(value: Boolean) extends Literal[Boolean]
  case class IntLiteral(value: Int) extends Literal[Int]
  case class DoubleLiteral(value: Double) extends Literal[Double]
  case class StringLiteral(value: String) extends Literal[String]

  case class Id(x: String) extends Term:
    override def toString: String = x

  case class Param(name: String, ty: Type) extends Term:
    override def toString: String = s"$name: $ty"

  case class Lam(params: Seq[Param], t: Term) extends Term:
    override def toString: String = s"(${params.mkString(", ")}) => $t"

  // TODO: Update toString
  case class App(fun: Term, args: Seq[Seq[Term]]) extends Term:
    override def toString: String = s"($fun)(${args.mkString(", ")})"

  case class AppInfix(t1: Term, op: String, t2: Term) extends Term:
    override def toString: String = s"$t1 $op $t2"

  case class AppUnary(t: Term, op: String) extends Term:
    override def toString: String = s"$op$t"

  case class Select(t: Term, name: String) extends Term:
    override def toString: String = s"$t.$name"

