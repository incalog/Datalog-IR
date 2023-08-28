package inca.ir.extension.bool

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, Term, Type}
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case BoolAtom(t) =>
      for (v <- visitTerm(t))
        yield BoolAtom(t)
    case _ => super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case term: BoolTerm => visitBoolTerm(term)
    case _ => super.visitTerm(term))

  def visitBoolTerm(term: BoolTerm): Seq[Term] = term match
    case AtomAsBool(a) =>
      for (v <- visitAtom(a))
        yield AtomAsBool(v)
    case BoolAnd(t1, t2) =>
      for (v1 <- visitTerm(t1); v2 <- visitTerm(t2))
        yield BoolAnd(v1, v2)
    case BoolOr(t1, t2) =>
      for (v1 <- visitTerm(t1); v2 <- visitTerm(t2))
        yield BoolOr(v1, v2)
    case BoolNot(t) =>
      for (v <- visitTerm(t))
        yield BoolNot(v)
    case BoolTrue => Seq(BoolTrue)
    case BoolFalse => Seq(BoolFalse)

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TBoolean => TBoolean
    case _ => super.visitType(ty))