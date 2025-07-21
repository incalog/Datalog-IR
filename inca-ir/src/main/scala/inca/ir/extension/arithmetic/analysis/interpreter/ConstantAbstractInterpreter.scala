package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Structural, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantIntV(value: Int) extends Value:
  override def toString: String = value.toString
  override def isConstant: Boolean = true

case class ConstantDoubleV(value: Double) extends Value:
  override def toString: String = value.toString
  override def isConstant: Boolean = true
  
given Structural[ConstantIntV] with {}
given Structural[ConstantDoubleV] with {}

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => Topped.Actual(i1 == i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => Topped.Actual(d1 == d2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantIntV(i1), ConstantIntV(i2)) => Topped.Actual(i1 != i2)
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) => Topped.Actual(d1 != d2)
    case _ => super.neq(v1, v2)

private def constantIntFromToppedInt(value: Topped[Int]): Value = value match
    case Topped.Top => Value.Top
    case Topped.Actual(i) => ConstantIntV(i)

private def constantDoubleFromToppedDouble(value: Topped[Double]): Value = value match
    case Topped.Top => Value.Top
    case Topped.Actual(d) => ConstantDoubleV(d)

private def toppedIntAsConstantInt(v: Value)(using except: Except[BaseIRException, ?, ?]): Topped[Int] = v match
    case ConstantIntV(i) => Topped.Actual(i)
    case Value.Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to int")

private def toppedDoubleAsConstantDouble(v: Value)(using except: Except[BaseIRException, ?, ?]): Topped[Double] = v match
    case ConstantDoubleV(d) => Topped.Actual(d)
    case Value.Top => Topped.Top
    case _ => throw IllegalArgumentException(s"Can not convert $v to double")

trait ConstantJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.combine(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantIntV(i1), ConstantIntV(i2)) if i1 == i2 => lhs
    case (ConstantDoubleV(d1), ConstantDoubleV(d2)) if d1 == d2 => lhs
    case _ => super.meet(lhs, rhs)

class ConstantArithmeticRefinementOps extends ArithmeticRefinementOps[Value]:
  override def refine(v1: Value, v2: Value, op: BinaryArithmeticComparisonOperator): (Value, Value) =
    // no refinement for constant values
    (v1, v2)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value] = new LiftedIntegerOps[Int, Value, Topped[Int]](toppedIntAsConstantInt(_)(using except), constantIntFromToppedInt) (
      using ToppedIntegerOps[Int, Int](using implicitly, failure, effects)
    )
    with IntOps[Int, Value]:
    override def integerValue(v: Value): Option[Int] = v match
      case ConstantIntV(i) => Some(i)
      case _ => None
      
  val doubleOps: FloatOps[Double, Value] = new LiftedFloatOps[Double, Value, Topped[Double]] (toppedDoubleAsConstantDouble(_)(using except), constantDoubleFromToppedDouble) (
    using ToppedFloatOps[Double, Double] (using implicitly) //  failure and effects are not needed... why?
  )
  
  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], Topped[Int], Topped[Boolean]](toppedIntAsConstantInt(_)(using except), identity)
  
  val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], Topped[Double], Topped[Boolean]](toppedDoubleAsConstantDouble(_)(using except), identity)
  
  val arithmeticRefinementOps: ArithmeticRefinementOps[Value] = ConstantArithmeticRefinementOps()
