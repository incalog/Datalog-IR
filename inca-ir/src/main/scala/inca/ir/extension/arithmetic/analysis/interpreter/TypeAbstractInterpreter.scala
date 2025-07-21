package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.TypeValue
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, BaseTypeFloatOps}
import sturdy.values.integer.{IntegerOps, LiftedIntegerOps, BaseTypeIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps}
import sturdy.values.types.{BaseType, given}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  override val intOps: IntOps[Int, Value] = new LiftedIntegerOps[Int, Value, BaseType[Int]](
    {case TypeValue(TInt) | Value.Top => BaseType[Int]},
    _ => TypeValue(TInt)
  ) with IntOps[Int, Value]:
    override def integerValue(v: Value): Option[Int] = None

  override val doubleOps: FloatOps[Double, Value] = new LiftedFloatOps[Double, Value, BaseType[Double]](
    { case TypeValue(TDouble) | Value.Top => BaseType[Double] },
    _ => TypeValue(TDouble)
  )
  override val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], BaseType[Int], BaseType[Boolean]](
    {case TypeValue(TInt) | Value.Top => BaseType[Int]},
    _ => Topped.Top
  )
  override val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], BaseType[Double], BaseType[Boolean]](
    { case TypeValue(TDouble) | Value.Top => BaseType[Double] },
    _ => Topped.Top
  )

  // Don't refine values
  override val arithmeticRefinementOps: ArithmeticRefinementOps[Value] =
    (v1: Value, v2: Value, op: BinaryArithmeticComparisonOperator) => (v1, v2)
