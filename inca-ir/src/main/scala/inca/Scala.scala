package inca

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

  // TODO: Fill the signature
  case class Id(x: String) extends Term:
    override def toString: String = x

  case class Lam(params: Seq[(String, Option[Type])], t: Term) extends Term:
    override def toString: String = {
      val paramsString = params.map { case (name, ty) =>
        if (ty.nonEmpty)
          s"$name: ${ty.get}"
        else
          s"$name"
      }
      s"(${paramsString.mkString(", ")}) => $t"
    }

  case class App(fun: Term, args: Seq[Term]) extends Term:
    override def toString: String = s"($fun)(${args.mkString(", ")})"

  case class AppInfix(t1: Term, op: String, t2: Term) extends Term:
    override def toString: String = s"$t1 $op $t2"

  case class Select(t: Term, name: String) extends Term

