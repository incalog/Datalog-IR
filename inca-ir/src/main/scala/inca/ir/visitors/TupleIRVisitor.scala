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

  override def visitType(ty: Type): Seq[Type] = ty match
    case TTuple(tys) => Seq(TTuple(tys.flatMap(visitType)))
    case _ => super.visitType(ty)