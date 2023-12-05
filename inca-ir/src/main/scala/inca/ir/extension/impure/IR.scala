package inca.ir.extension.impure

import inca.ir.extension.arithmetic
import inca.ir.extension.demand
import inca.ir.{Atom, BaseIR, Language, ModuleEntry, Name, Term, Type, Var}

object IR extends IR { }
trait IR extends BaseIR:
  override val name: String = "Impure"
  override def language: Language = super.language + IR
  override def requires: Language = Language(arithmetic.IR, demand.IR)

trait ImpurityKind:
  val name: String
  val ty: Type
  override def toString: String = name

case class Impure(v: Name, atoms: Seq[Atom], update: Term, kind: ImpurityKind) extends Atom with Var.Target:
  override def vars: Seq[Var] = Var(v) +: (atoms.flatMap(_.vars) ++ update.vars)
  override def toString: String = s"Impure($v => ${atoms.mkString(", ")}, $update)"

object Impure:
  def apply(v: Name, atom: Atom, update: Term, kind: ImpurityKind): Impure =
    new Impure(v, Seq(atom), update, kind)

  def counter(v: Name, atom: Atom, kind: ImpurityKind): Impure =
    import inca.ir.extension.arithmetic.*
    new Impure(v, Seq(atom), Add(Var(v), IntNum(1)), kind)