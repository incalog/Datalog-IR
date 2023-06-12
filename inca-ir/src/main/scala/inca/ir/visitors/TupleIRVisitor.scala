package inca.ir.visitors

import inca.ir
import inca.ir.extensions.*
import inca.ir.*

import scala.collection.immutable.Seq

trait TupleIRVisitor extends BaseIRVisitor:
  override def visitTerm(term: Term): Seq[Term] = term match
    case Project(t, idx) => Seq(Project(visitTerm(t).head, idx))
    case Tuple(ts) => Seq(Tuple(ts.flatMap(visitTerm)))
    case _ => super.visitTerm(term)

  override def visitType(ty: Type): Type = ty match
    case TTuple(tys) => TTuple(tys.map(visitType))
    case _ => super.visitType(ty)