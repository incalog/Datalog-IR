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

  trait Literal[T](x: T) extends Term:
    override def toString: String = x.toString

  case class BoolLiteral(x: Boolean) extends Literal[Boolean](x)
  case class IntLiteral(x: Int) extends Literal[Int](x)
  case class DoubleLiteral(x: Double) extends Literal[Double](x)
  case class StringLiteral(x: String) extends Literal[String](x)

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

