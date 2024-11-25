package inca.ir.extension.string.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.extension.string.{StringLit, StringConcat, ToString}
import sturdy.data.MayJoin

trait StringOps[V]:
  def stringLit(s: String): V

  def toString(v: V): V

  def concat(v1: V, v2: V): V

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val stringOps: StringOps[V]

  override def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case StringLit(s) => relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(stringOps.stringLit(s))))
    case ToString(t) =>
      val rv = evalTerm(t)
      val trv = relationOps.projectAndRename(rv, Map(RESULT_COLUMN -> LHS_COLUMN))
      relationOps.project(
        relationOps.map(trv, RESULT_COLUMN) { case Seq(e) => stringOps.toString(e) },
        Seq(RESULT_COLUMN)
      )
    case StringConcat(lhs, rhs) =>
      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)
      val combinations = relationOps.cartesian(
        relationOps.rename(ls, Map(RESULT_COLUMN -> LHS_COLUMN)),
        relationOps.rename(rs, Map(RESULT_COLUMN -> RHS_COLUMN))
      )
      relationOps.project(
        relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => stringOps.concat(l, r) },
        Seq(RESULT_COLUMN)
      )
    case _ => super.evalTermOpen(term)
