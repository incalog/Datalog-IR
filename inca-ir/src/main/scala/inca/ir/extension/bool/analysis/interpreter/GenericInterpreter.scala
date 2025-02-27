package inca.ir.extension.bool.analysis.interpreter

import inca.ir
import inca.ir.*
import inca.ir.analysis.base.effect.BaseIRFailure
import inca.ir.analysis.base.interpreter.{BaseGenericInterpreter, SupColumn}
import inca.ir.extension.bool.*
import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps
import sturdy.data.MakeJoined
import sturdy.values.{Join, MaybeChanged, Topped}
import sturdy.values.booleans.given_Structural_Boolean
import sturdy.values.JoinToppedFlat

case object InvalidBooleanOp extends BaseIRFailure

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val booleanOps: BooleanOps[V]

  override def evalTermOpen(term: ir.Term)(using Fixed): SupColumn = term match
    case AtomAsBool(a) =>
      val res = except.tryCatch {
        evalAtom(a)
        Topped.Actual(true)
      } /* catch */ { exec =>
        Topped.Actual(false)
      }
      if (res.isActual)
        termResult(booleanOps.boolLit(res.get))
      else
        // Should never happen in the concrete case
        termResult(topV)
    case BoolAnd(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(booleanOps.and)
    case BoolOr(t1, t2) => binaryOp(evalTerm(t1), evalTerm(t2))(booleanOps.or)
    case BoolNot(t) => unaryOp(evalTerm(t))(booleanOps.not)
    case BoolTrue => termResult(booleanOps.boolLit(true))
    case BoolFalse => termResult(booleanOps.boolLit(false))
    case _ => super.evalTermOpen(term)

