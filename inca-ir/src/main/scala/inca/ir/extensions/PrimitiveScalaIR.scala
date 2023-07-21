package inca.ir.extensions

import inca.Scala
import inca.ir.*


case class TPrimitive(ty: Scala.Type) extends Type

// TODO: Eval instead of Constant and Application ?
case class Constant(value: Scala.Term) extends Term
case class Application(out: Term, fun: Scala.Term, args: Seq[Term]) extends Atom


trait PrimitiveScalaIR extends BaseIR:
  override val name: String = "PrimitiveScala"
  override def language: Language = super.language + new PrimitiveScalaIR {}
  // This IR is not reducible
  override def requires: Language = Language(new PrimitiveScalaIR {})

object PrimitiveScalaIR extends PrimitiveScalaIR { }