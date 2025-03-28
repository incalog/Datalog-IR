package inca.ir.extension.set

import inca.ir.*

case class TSet(ty: Type) extends Type:
  override def toString: String = s"TSet[$ty]"

case class SetLit(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("Set(", ", ", ")")
  override def vars: Seq[Var] = ts.flatMap(_.vars)
  override def commonVars: Set[Var] = ts.flatMap(_.commonVars).toSet

object SetLit:
  def from(ts: Term*): SetLit = new SetLit(ts)
  def empty: SetLit = new SetLit(Seq())

/** Wraps a named relation as a set of tuples */
case class SetFrom(ref: Ref[Relation]) extends Term:
  override def toString: String = s"Set.from(${ref.name})"
  override def vars: Seq[Var] = Seq()
  override def commonVars: Set[Var] = Set()

case class SetUnion(ts: Seq[Term]) extends Term:
  override def toString: String = ts.mkString("(", " ∪ ", ")")
  override def vars: Seq[Var] = ts.flatMap(_.vars)
  override def commonVars: Set[Var] = ts.flatMap(_.commonVars).toSet

object SetUnion:
  def apply(t1: Term, t2: Term): SetUnion = SetUnion(Seq(t1, t2))


/** Can desugar to Set(ts | ts in $t1, ts in $t2) */
case class SetIntersection(t1: Term, t2: Term) extends Term:
  override def toString: String = s"($t1 ∩ $t2)"
  override def vars: Seq[Var] = t1.vars ++ t2.vars
  override def commonVars: Set[Var] = t1.commonVars ++ t2.commonVars

case class SetComprehension(elem: Term, atoms: Seq[Atom]) extends Term:
  override def toString: String = s"Set($elem | ${atoms.mkString(", ")})"
  override def vars: Seq[Var] = elem.vars ++ atoms.flatMap(_.vars)
  override def commonVars: Set[Var] = elem.commonVars ++ atoms.flatMap(_.commonVars)

// TODO: Use arguments instead of term to support wildcards
case class SetMember(mem: Term, s: Term) extends Atom:
  override def toString: String = s"($mem in $s)"
  override def vars: Seq[Var] = mem.vars ++ s.vars
  override def commonVars: Set[Var] = mem.commonVars ++ s.commonVars

trait IR extends BaseIR:
  override val name: String = "Set"
  override def language: Language = super.language + new IR {}
  override def requires: Language = Language()

object IR extends IR {}