package inca.ir.extension.string.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.string.{StringConcat, StringLit, ToString}
import sturdy.data.MayJoin

trait StringOps[V]:
  def stringLit(s: String): V

  def toString(v: V): V

  def concat(v1: V, v2: V): V

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val stringOps: StringOps[V]

  override protected def canDetermineValue(t: ir.Term): Boolean = t match
    case StringLit(_) => true
    case StringConcat(lhs, rhs) => canDetermineValue(lhs) && canDetermineValue(rhs)
    case ToString(t) => canDetermineValue(t)
    case _ => super.canDetermineValue(t)

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case StringLit(s) => termResult(stringOps.stringLit(s))
    case ToString(t) => unaryOp(evalTerm(t))(stringOps.toString)
    case StringConcat(lhs, rhs) => binaryOp(evalTerm(lhs), evalTerm(rhs))(stringOps.concat)
    case _ => super.evalTermOpen(term)
