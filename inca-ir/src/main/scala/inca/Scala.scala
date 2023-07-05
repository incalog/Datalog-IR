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

  case class IntLiteral(x: Int) extends Literal[Int](x)
  case class DoubleLiteral(x: Double) extends Literal[Double](x)

  // TODO: Fill the signature
  case class Fun() extends Term
