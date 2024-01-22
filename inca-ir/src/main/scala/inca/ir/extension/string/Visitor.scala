package inca.ir.extension.string

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Term, Type}

trait Visitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case StringLit(s) => Seq(StringLit(s))
    case StringConcat(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map(StringConcat.apply)
    case ToString(t) =>
      visitTerm(t).map(ToString.apply)
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type =  preserveHints(ty)(ty match
    case TString => TString
    case _ => super.visitType(ty))
