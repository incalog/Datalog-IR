package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{AType, TypeRelation, Value}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, TypeFloatOps}
import sturdy.values.integer.{IntegerOps, LiftedIntegerOps, TypeIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps}
import sturdy.values.types.{BaseType, given}

trait TypeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], TypeRelation, Powerset[BaseIRException], WithJoin]:

  override val intOps: IntegerOps[Int, Value] = new LiftedIntegerOps[Int, Value, BaseType[Int]](
    {case AType(TInt) | Value.Top => BaseType[Int]},
    _ => AType(TInt)
  )
  override val doubleOps: FloatOps[Double, Value] = new LiftedFloatOps[Double, Value, BaseType[Double]](
    { case AType(TDouble) | Value.Top => BaseType[Double] },
    _ => AType(TDouble)
  )
  override val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], BaseType[Int], BaseType[Boolean]](
    {case AType(TInt) | Value.Top => BaseType[Int]},
    _ => Topped.Top
  )
  override val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], BaseType[Double], BaseType[Boolean]](
    { case AType(TDouble) | Value.Top => BaseType[Double] },
    _ => Topped.Top
  )
