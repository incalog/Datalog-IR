package inca.ir.extension.bool

import inca.ir.Hint.preserveHints
import inca.ir.{Atom, Term, Type}
import inca.ir.visitors.BaseIRVisitor

trait Visitor extends BaseIRVisitor:
  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case BoolAtom(t) =>
      val ts = visitTerm(t)
      ts.map(BoolAtom.apply)
    case _ => super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case term: BoolTerm => visitBoolTerm(term)
    case _ => super.visitTerm(term))

  def visitBoolTerm(term: BoolTerm): Seq[Term] = term match
    case AtomAsBool(a) =>
      val as = visitAtom(a)
      as.map(AtomAsBool.apply)
    case BoolAnd(t1, t2) =>
      visitTerm(t1).zip(visitTerm(t2)).map(BoolAnd.apply)
    case BoolOr(t1, t2) =>
      visitTerm(t1).zip(visitTerm(t2)).map(BoolOr.apply)
    case BoolNot(t) =>
      for (v <- visitTerm(t))
        yield BoolNot(v)
    case BoolTrue => Seq(BoolTrue)
    case BoolFalse => Seq(BoolFalse)

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TBoolean => TBoolean
    case _ => super.visitType(ty))