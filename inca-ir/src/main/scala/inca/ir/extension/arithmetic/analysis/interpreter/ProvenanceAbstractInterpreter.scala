package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ProvenanceAbstractRelation, ProvenanceV, Value}
import sturdy.data.WithJoin
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.values.integer.{LiftedIntegerOps, ToppedIntegerOps, given}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}


private val RuntimeIntV: Value = ProvenanceV("runtime.int")
private val RuntimeDoubleV: Value = ProvenanceV("runtime.double")

private def intToRuntime(value: Topped[Int]): Value = RuntimeIntV
private def runtimeToInt(value: Value): Topped[Int] = Topped.Top
private def doubleToRuntime(value: Topped[Double]): Value = RuntimeDoubleV
private def runtimeToDouble(value: Value): Topped[Double] = Topped.Top

class ProvenanceIntOps(using f: sturdy.effect.failure.Failure, eff: sturdy.effect.EffectStack)
  extends LiftedIntegerOps[Int, Value, Topped[Int]](runtimeToInt, intToRuntime)
    with IntOps[Int, Value]:
  override def integerValue(v: Value): Option[Int] = None


class ProvenanceArithmeticRefinementOps extends ArithmeticRefinementOps[Value]:
  override def refine(v1: Value, v2: Value, op: BinaryArithmeticComparisonOperator): (Value, Value) =
    (v1, v2)


trait ProvenanceAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ProvenanceAbstractRelation, Powerset[BaseIRException], WithJoin]:
  override val intOps: IntOps[Int, Value] = ProvenanceIntOps()
  override val doubleOps: FloatOps[Double, Value] =
    LiftedFloatOps[Double, Value, Topped[Double]](runtimeToDouble, doubleToRuntime)
  override val intOrderingOps: OrderingOps[Value, Topped[Boolean]] =
    LiftedOrderingOps[Value, Topped[Boolean], Topped[Int], Topped[Boolean]](runtimeToInt, identity)
  override val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] =
    LiftedOrderingOps[Value, Topped[Boolean], Topped[Double], Topped[Boolean]](runtimeToDouble, identity)
  override val arithmeticRefinementOps: ArithmeticRefinementOps[Value] = ProvenanceArithmeticRefinementOps()
