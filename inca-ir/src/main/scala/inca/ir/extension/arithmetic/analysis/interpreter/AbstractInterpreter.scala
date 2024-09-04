package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.{Atom, Term, Name}
import inca.ir.analysis.base.interpreter.BaseAbstractInterpreter
import inca.ir.analysis.base.values.{RelationValue, VBool, Value}
import inca.ir.extension.arithmetic.analysis.ordering.{DoubleVOrderingOps, IntVOrderingOps}
import inca.ir.extension.arithmetic.{BinCompare, BinOp, DoubleNum, IntNum, TDouble, TInt, UnOp}
import inca.ir.extension.arithmetic.analysis.values.{DoubleVOps, IntVOps}
import sturdy.effect.failure.Failure
import sturdy.values.integer.IntegerOps
import sturdy.values.floating.FloatOps
import sturdy.values.ordering.OrderingOps

// Constant Analysis
trait ConstantAbstractInterpreter extends AbstractInterpreter[Name, Value, VBool, RelationValue[Name, Value]]:
  val intOps: IntegerOps[Int, Value] = IntVOps(using failure, effects)
  val doubleOps: FloatOps[Double, Value] = DoubleVOps(using failure, effects)
  val intOrderingOps: OrderingOps[Value, VBool] = IntVOrderingOps()
  val doubleOrderingOps: OrderingOps[Value, VBool] = DoubleVOrderingOps()

trait AbstractInterpreter[C, V, B, RV] extends BaseAbstractInterpreter[C, V, B, RV]:
  val intOps: IntegerOps[Int, V]
  val doubleOps: FloatOps[Double, V]
  val intOrderingOps: OrderingOps[V, B]
  val doubleOrderingOps: OrderingOps[V, B]
