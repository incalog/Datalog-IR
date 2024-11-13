package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.analysis.base.values.{RelationValue, VBool, Value}
import inca.ir.extension.arithmetic.analysis.ordering.{DoubleVOrderingOps, IntVOrderingOps}
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.arithmetic.analysis.values.{DoubleVOps, IntVOps}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.failure.Failure
import sturdy.values.integer.IntegerOps
import sturdy.values.floating.FloatOps
import sturdy.values.ordering.OrderingOps

// Constant Analysis
trait ConstantAbstractInterpreter[J[_] <: MayJoin[_]] extends GenericInterpreter[Value, VBool, RelationValue[Value], J]:
  val intOps: IntegerOps[Int, Value] = IntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = DoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, VBool] = IntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, VBool] = DoubleVOrderingOps()

trait GenericInterpreter[V, B, RV, J[_] <: MayJoin[_]] extends BaseGenericInterpreter[V, B, RV, J]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]

  override def evalTermOpen(term: ir.Term)(using Fixed): Seq[V] = term match
    case IntNum(i: Int) => Seq(intOps.integerLit(i))
    case DoubleNum(d: Double) => Seq(doubleOps.floatingLit(d))
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TInt) =>
      val ls = evalTermOpen(lhs)
      val rs = evalTermOpen(rhs)
      val combinations = cartesian(ls, rs)
      val values = op match
        case "+" => relationOps.map(combinations) { case Seq(l, r) => intOps.add(l, r) }
        case "-" => relationOps.map(combinations) { case Seq(l, r) => intOps.sub(l, r) }
        case "*" => relationOps.map(combinations) { case Seq(l, r) => intOps.mul(l, r) }
        case "/" => relationOps.map(combinations) { case Seq(l, r) => intOps.div(l, r) }
        case "%" => relationOps.map(combinations) { case Seq(l, r) => intOps.remainder(l, r) }
        case "min" => relationOps.map(combinations) { case Seq(l, r) => intOps.min(l, r) }
        case "max" => relationOps.map(combinations) { case Seq(l, r) => intOps.max(l, r) }
      values.iterator.toSeq
    case BinOp(lhs, rhs, op) if term.typ.exists(_.ty == TDouble) =>
      val ls = evalTermOpen(lhs)
      val rs = evalTermOpen(rhs)
      val combinations = cartesian(ls, rs)
      val values = op match
        case "+" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.add(l, r) }
        case "-" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.sub(l, r) }
        case "*" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.mul(l, r) }
        case "/" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.div(l, r) }
        case "min" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.min(l, r) }
        case "max" => relationOps.map(combinations) { case Seq(l, r) => doubleOps.max(l, r) }
      values.iterator.toSeq
    case _ => super.evalTermOpen(term)
