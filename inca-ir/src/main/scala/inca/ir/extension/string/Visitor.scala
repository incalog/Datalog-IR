package inca.ir.extension.string

import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Term, Type}

trait Visitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case StringLit(_) => Seq(term)
    case StringConcat(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(lhs)).map(StringConcat.apply)
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type =  preserveHints(ty)(ty match
    case TString => TString
    case _ => super.visitType(ty))
