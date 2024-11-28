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
    case StringLit(s) => termResult(stringOps.stringLit(s))
    case ToString(t) => unaryOp(evalTerm(t))(stringOps.toString)
    case StringConcat(lhs, rhs) => binaryOp(evalTerm(lhs), evalTerm(rhs))(stringOps.concat)
    case _ => super.evalTermOpen(term)
