package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.Atom
import inca.ir.analysis.base.effect.AtomFailed
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.analysis.base.values.{ARelationValue, VBool, Value}
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.failure.Failure
import sturdy.values.integer.IntegerOps
import sturdy.values.floating.FloatOps
import sturdy.values.ordering.OrderingOps

trait GenericInterpreter[V, B, RV, ExcV, J[_] <: MayJoin[?]] extends BaseGenericInterpreter[V, B, RV, ExcV, J]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]

  override def evalAtomOpen(at: Atom)(using Fixed): Unit = at match
    case BinCompare(lhs, rhs, op)  =>
      val orderingOps = lhs.typ match
        case Some(tt) if tt.ty == TInt => intOrderingOps
        case Some(tt) if tt.ty == TDouble => doubleOrderingOps
        case tt => throw IllegalStateException(s"Unexpected TermType $tt")

      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)
      val combinations = relationOps.cartesian(
        relationOps.rename(ls, Map(RESULT_COLUMN -> LHS_COLUMN)),
        relationOps.rename(rs, Map(RESULT_COLUMN -> RHS_COLUMN))
      )

      // Note, this is the complement on purpose. We want to find all bindings we need to remove. 
      val comparisonResults = op match
        case "<=" => relationOps.filter(combinations) { case Seq(l, r) => orderingOps.gt(l, r) }
        case "<" => relationOps.filter(combinations) { case Seq(l, r) => orderingOps.ge(l, r) }
        case ">" => relationOps.filter(combinations) { case Seq(l, r) => orderingOps.le(l, r) }
        case ">=" => relationOps.filter(combinations) { case Seq(l, r) => orderingOps.lt(l, r) }

      branchOps.boolBranch(relationOps.isEmpty(comparisonResults)) {
        // All succeeded
      } {
        // At least one failed => filter
        val mapping = extractVarName(lhs).map(LHS_COLUMN -> _.name) ++ extractVarName(rhs).map(RHS_COLUMN -> _.name)
        mergeIntoEnv(relationOps.projectAndRename(comparisonResults, mapping.toMap), true)
      }

    case _ => super.evalAtomOpen(at)

  override def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case IntNum(i: Int) => relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(intOps.integerLit(i))))
    case DoubleNum(d: Double) => relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(doubleOps.floatingLit(d))))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TInt) =>
      // TODO: Single scan for these 3 operations
      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)
      val combinations = relationOps.cartesian(
        relationOps.rename(ls, Map(RESULT_COLUMN -> LHS_COLUMN)),
        relationOps.rename(rs, Map(RESULT_COLUMN -> RHS_COLUMN))
      )
      val values = op match
        case "+" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.add(l, r) }
        case "-" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.sub(l, r) }
        case "*" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.mul(l, r) }
        case "/" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.div(l, r) }
        case "%" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.remainder(l, r) }
        case "min" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.min(l, r) }
        case "max" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => intOps.max(l, r) }
      relationOps.project(values, Seq(RESULT_COLUMN))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TDouble) =>
      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)
      val combinations = relationOps.cartesian(
        relationOps.rename(ls, Map(RESULT_COLUMN -> LHS_COLUMN)),
        relationOps.rename(rs, Map(RESULT_COLUMN -> RHS_COLUMN))
      )
      val values = op match
        case "+" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.add(l, r) }
        case "-" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.sub(l, r) }
        case "*" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.mul(l, r) }
        case "/" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.div(l, r) }
        case "min" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.min(l, r) }
        case "max" => relationOps.map(combinations, RESULT_COLUMN) { case Seq(l, r) => doubleOps.max(l, r) }
      relationOps.project(values, Seq(RESULT_COLUMN))
    case _ => super.evalTermOpen(term)
