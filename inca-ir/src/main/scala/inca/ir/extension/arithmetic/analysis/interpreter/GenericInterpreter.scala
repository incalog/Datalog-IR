package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.analysis.base.values.{RelationValue, VBool, Value}
import inca.ir.extension.arithmetic.analysis.ordering.{DoubleVOrderingOps, IntVOrderingOps}
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.arithmetic.analysis.values.{DoubleVOps, IntVOps}
import sturdy.effect.failure.Failure
import sturdy.values.integer.IntegerOps
import sturdy.values.floating.FloatOps
import sturdy.values.ordering.OrderingOps

// Constant Analysis
trait ConstantAbstractInterpreter extends GenericInterpreter[ir.Name, Value, VBool, RelationValue[ir.Name, Value]]:
  val intOps: IntegerOps[Int, Value] = IntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = DoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, VBool] = IntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, VBool] = DoubleVOrderingOps()

trait GenericInterpreter[C, V, B, RV] extends BaseGenericInterpreter[C, V, B, RV]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]

  override def evalTermExtend(term: ir.Term)(using Fixed): (Seq[V], B) = term match
    case IntNum(i: Int) => (Seq(intOps.integerLit(i)), boolTrue)
    case DoubleNum(d: Double) => (Seq(doubleOps.floatingLit(d)), boolTrue)
    case _ => super.evalTermExtend(term)
