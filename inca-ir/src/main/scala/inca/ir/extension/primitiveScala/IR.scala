package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.*


case class TScala(ty: Scala.Type) extends Type

// TODO: Eval instead of Constant and Application ?
case class Constant(value: Scala.Term) extends Term
case class Application(out: Term, fun: Scala.Term, args: Seq[Term]) extends Atom


trait IR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new IR {}
  // This IR is not reducible
  override def requires: Language = Language(new IR {})

object IR extends IR { }