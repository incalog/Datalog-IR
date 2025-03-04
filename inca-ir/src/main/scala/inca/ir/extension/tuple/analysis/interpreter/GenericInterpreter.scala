package inca.ir.extension.tuple.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.BaseIRFailure
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.tuple.{Project, TupleLit}
import sturdy.data.MayJoin

case object InvalidTupleProjection extends BaseIRFailure

trait TupleOps[V]:
  def tupleLit(ts: Seq[V]): V
  def project(t: V, index: Int): V
  def iter(t: V): Seq[V]

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val tupleOps: TupleOps[V]

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case TupleLit(ts) => naryOp(ts.map(evalTerm))(tupleOps.tupleLit)
    case Project(t, idx) => unaryOp(evalTerm(t))(tupleOps.project(_, idx))
    case _ => super.evalTermOpen(term)

