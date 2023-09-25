package inca.ir.extension.tuple

import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case Project(t, idx) => visitTerm(t).map(pt => Project(pt, idx))
      case TupleLit(ts) => Seq(TupleLit(ts.flatMap(visitTerm)))
      case _ => super.visitTerm(term)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TTuple(tys) => TTuple(tys.map(visitType))
      case _ => super.visitType(ty)
  }