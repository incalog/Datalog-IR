package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.*
import scala.quoted.{Type => MetaType, Expr => MetaExpr}

case class TScala(ty: Scala.Type) extends Type
//case class TScala(ty: MetaType[Any]) extends Type

// TODO: Eval instead of Constant and Application ?
case class Constant(value: Scala.Term) extends Term
//case class Constant(value: MetaExpr[Any]) extends Term
//case class Application0[A](fun: MetaExpr[Function0[A]], args: A) extends Term
//case class Application1[A, B](fun: MetaExpr[Function[A, B]], args: (A, B)) extends Term
//case class Application(fun: MetaExpr[Any], args: Seq[Term]) extends Term
case class Application(out: Term, fun: Scala.Term, args: Seq[Term]) extends Atom

/*object Application:
  def apply(fun: MetaExpr[Any], args: Seq[Term]) = {
    ???
  }*/


trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // We can not lower this IR any further
  override def requires: Language = Language(new IR {})

object IR extends IR { }