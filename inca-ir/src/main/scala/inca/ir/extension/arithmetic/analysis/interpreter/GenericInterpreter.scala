package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
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

  // TODO: Add atoms

  override def evalTermOpen(term: ir.Term)(using Fixed): RV = term match
    case IntNum(i: Int) => relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(intOps.integerLit(i))))
    case DoubleNum(d: Double) => relationOps.make(Seq(RESULT_COLUMN), Seq(Seq(doubleOps.floatingLit(d))))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TInt) =>
      // TODO: Single scan for these 3 operations
      val ls = evalTerm(lhs)
      val rs = evalTerm(rhs)
      val combinations = relationOps.cartesian(
        relationOps.rename(ls, Map(RESULT_COLUMN -> "lhs")),
        relationOps.rename(rs, Map(RESULT_COLUMN -> "rhs"))
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
        relationOps.rename(ls, Map(RESULT_COLUMN -> "lhs")),
        relationOps.rename(rs, Map(RESULT_COLUMN -> "rhs"))
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
