package inca.ir.extensions

import inca.Scala
import inca.ir.*


case class TPrimitive(ty: Scala.Type) extends Type

// TODO: Eval instead of Constant and Application ?
case class Constant(value: Scala.Term) extends Term
case class Application(out: Term, fun: Scala.Fun, args: Seq[Term]) extends Atom


//Rename PrimitiveScalaIR
trait PrimitiveIR extends BaseIR:
  override val name: String = "Primitive"
  override def language: Language = super.language + new PrimitiveIR {}
  // This IR is not reducible
  override def requires: Language = Language(this)
