package inca.ir.extension.demand

import inca.ir.{Atom, BaseIR, Language, Name, Term, Type, Var}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Demand"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class TDemand(ty: Type) extends Type:
  override def flatten: Seq[Type] = ty.flatten.map(TDemand.apply)
  override def toString: String = s"TDemand($ty)"

def demandRelationName(rel: Name): Name =
  Name(s"$rel$$input")
