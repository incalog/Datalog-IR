package inca.foreign.scala.syntax

object Scala:
  trait Tree

  trait Type extends Tree

  case class TypeName(s: String) extends Type:
    override def toString: String = s

  
  case class FunType(args: Seq[Type], ret: Type) extends Type:
    override def toString: String = s"(${args.mkString(", ")}) => $ret"

  //case class TupleType(tys: Seq[Type]) extends Type:
  //  override def toString: String = s"(${tys.mkString(", ")})"

  trait Stat extends Tree
  trait Defn extends Stat

  // Defn

  // TODO: complete this
  // case class Object(name: String) extends Defn


  // Terms

  trait Term extends Stat

  case class Var(x: String) extends Term:
    override def toString: String = x

  trait Literal[T] extends Term:
    val value: T
    override def toString: String = value.toString

  case class BoolLiteral(value: Boolean) extends Literal[Boolean]
  case class IntLiteral(value: Int) extends Literal[Int]
  case class DoubleLiteral(value: Double) extends Literal[Double]
  case class StringLiteral(value: String) extends Literal[String]

  // TODO: Complete these cases
  case class Id(x: String) extends Term:
    override def toString: String = x

  case class Param(name: String, ty: Type):
    override def toString: String = s"$name: $ty"

  case class Lam(params: Seq[Param], t: Term) extends Term:
    override def toString: String = s"(${params.mkString(", ")}) => $t"

  case class App(fun: Term, args: Seq[Term]) extends Term:
    override def toString: String = s"($fun)(${args.mkString(", ")})"

  case class AppInfix(t1: Term, op: String, t2: Term) extends Term:
    override def toString: String = s"$t1 $op $t2"

  case class AppUnary(t: Term, op: String) extends Term:
    override def toString: String = s"$op$t"

  case class Select(t: Term, name: String) extends Term

