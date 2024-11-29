package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.values.{TypeRelation, TypeValue, Value}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.failure.Failure
import sturdy.values.Topped
import sturdy.values.floating.{FloatOps, LiftedFloatOps, TypeFloatOps}
import sturdy.values.integer.{IntegerOps, LiftedIntegerOps, TypeIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps}
import sturdy.values.types.{BaseType, given}

trait TypeAbstractInterpreter extends GenericInterpreter[TypeValue, Topped[Boolean], TypeRelation, Unit, WithJoin]:
  given Failure = failure
  override val intOps: IntegerOps[Int, TypeValue] = new LiftedIntegerOps[Int, TypeValue, BaseType[Int]](
    {case TypeValue.AType(TInt) => BaseType[Int]},
    _ => TypeValue.AType(TInt)
  )
  override val doubleOps: FloatOps[Double, TypeValue] = new LiftedFloatOps[Double, TypeValue, BaseType[Double]](
    { case TypeValue.AType(TDouble) => BaseType[Double] },
    _ => TypeValue.AType(TDouble)
  )
  override val intOrderingOps: OrderingOps[TypeValue, Topped[Boolean]] = new LiftedOrderingOps[TypeValue, Topped[Boolean], BaseType[Int], BaseType[Boolean]](
    {case TypeValue.AType(TInt) => BaseType[Int]},
    _ => Topped.Top
  )
  override val doubleOrderingOps: OrderingOps[TypeValue, Topped[Boolean]] = new LiftedOrderingOps[TypeValue, Topped[Boolean], BaseType[Double], BaseType[Boolean]](
    { case TypeValue.AType(TInt) => BaseType[Double] },
    _ => Topped.Top
  )
