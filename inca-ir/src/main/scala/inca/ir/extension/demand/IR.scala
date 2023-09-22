package inca.ir.extension.demand

import inca.ir.{Atom, BaseIR, Language, Name, Term, Var}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Demand"
  override def language: Language = super.language + IR
  override def requires: Language = Language()

case class Demand(ts: Seq[Term]) extends Atom:
  override def toString: String = s"Demand(${ts.mkString(", ")})"
  override def vars: Seq[Var] = ts.flatMap(_.vars)

object Demand:
  def apply(t: Term, ts: Term*): Demand = new Demand(t +: ts)

def demandRelationName(rel: Name): Name =
  Name(s"$rel$$input")
