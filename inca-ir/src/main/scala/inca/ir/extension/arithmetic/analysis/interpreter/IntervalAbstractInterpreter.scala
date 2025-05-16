package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.Term
import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, BaseWidenV, Value}
import inca.ir.visitors.IRVisitor
import inca.ir.extension.arithmetic as irarith
import sturdy.values.{Powerset, Topped}
import sturdy.data.{JOptionC, MakeJoined, MayJoin, WithJoin}
import sturdy.data.MayJoin.NoJoin
import sturdy.values.integer.{ConcreteIntegerOps, ConcreteStrictIntegerOps, IntegerOps, LiftedIntegerOps, NumericInterval, NumericIntervalEqOps, NumericIntervalIntegerOps, NumericIntervalJoin, NumericIntervalOrderingOps, NumericIntervalWiden, StandardIntervalIntegerOps, StrictIntegerOps, TopNumericIntervalInt}
import sturdy.values.ordering.{EqOps, LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.effect.EffectStack
import sturdy.effect.except.Except
import sturdy.effect.failure.Failure
import sturdy.values.floating.{FloatOps, LiftedFloatOps}

import scala.math.Ordering.given

type IntInterval = NumericInterval[Int]
type DoubleInterval = NumericInterval[Double]

val topIntInterval: IntInterval = NumericInterval.safe(Integer.MIN_VALUE, Integer.MAX_VALUE)
val topDoubleInterval: DoubleInterval = NumericInterval.safe(Double.MinValue, Double.MaxValue)

case class IntervalIntV(iv: IntInterval) extends Value:
  override def toString: String = iv.toString
  override def isConstant: Boolean = iv.isConstant

object IntervalIntV:
  def constant(i: Int): IntervalIntV = new IntervalIntV(NumericInterval.constant(i))

case class IntervalDoubleV(iv: DoubleInterval) extends Value:
  override def toString: String = iv.toString
  override def isConstant: Boolean = iv.isConstant

object IntervalDoubleV:
  def constant(d: Double): IntervalDoubleV = new IntervalDoubleV(NumericInterval.constant(d))

trait IntervalEqOps extends BaseEqOps:
  val intIntervalOps = new NumericIntervalEqOps[Int]
  val doubleIntervalOps = new NumericIntervalEqOps[Double]

  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) => intIntervalOps.equ(iv1, iv2)
    case (IntervalDoubleV(iv1), IntervalDoubleV(iv2)) => doubleIntervalOps.equ(iv1, iv2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) => intIntervalOps.neq(iv1, iv2)
    case (IntervalDoubleV(iv1), IntervalDoubleV(iv2)) => doubleIntervalOps.neq(iv1, iv2)
    case _ => super.neq(v1, v2)

private def numericDoubleIntervalToValue(value: DoubleInterval): Value = value match
  case iv if iv == topDoubleInterval => Value.Top
  case iv => IntervalDoubleV(iv)

private def valueAsNumericDoubleInterval(v: Value)(using except: Except[BaseIRException, ?, ?]): DoubleInterval = v match
  case IntervalDoubleV(iv) => iv
  case Value.Top => topDoubleInterval
  case _ => throw IllegalArgumentException(s"Can not convert $v to int")

private class IntervalDoubleVOps(using failure: Failure, effects: EffectStack, except: Except[BaseIRException, ?, ?])
  extends LiftedFloatOps[Double, Value, DoubleInterval](valueAsNumericDoubleInterval, numericDoubleIntervalToValue) (
    using new FloatOps[Double, DoubleInterval] {
      // TODO: Implement these
      override def floatingLit(f: Double): DoubleInterval = ???
      override def randomFloat(): DoubleInterval = ???
      override def add(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def sub(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def mul(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def div(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def min(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def max(v1: DoubleInterval, v2: DoubleInterval): DoubleInterval = ???
      override def absolute(v: DoubleInterval): DoubleInterval = ???
      override def negated(v: DoubleInterval): DoubleInterval = ???
      override def sqrt(v: DoubleInterval): DoubleInterval = ???
      override def ceil(v: DoubleInterval): DoubleInterval = ???
      override def floor(v: DoubleInterval): DoubleInterval = ???
      override def truncate(v: DoubleInterval): DoubleInterval = ???
      override def nearest(v: DoubleInterval): DoubleInterval = ???
      override def copysign(v: DoubleInterval, sign: DoubleInterval): DoubleInterval = ???
    }
  )

private def numericIntIntervalToValue(value: IntInterval): Value = value match
  case iv if iv == topIntInterval => Value.Top
  case iv => IntervalIntV(iv)

private def valueAsNumericIntInterval(v: Value)(using except: Except[BaseIRException, ?, ?]): IntInterval = v match
  case IntervalIntV(iv) => iv
  case Value.Top => topIntInterval
  case _ => throw IllegalArgumentException(s"Can not convert $v to int")


private class IntervalIntVOps(using failure: Failure, effects: EffectStack, except: Except[BaseIRException, ?, ?])
  extends LiftedIntegerOps[Int, Value, IntInterval](valueAsNumericIntInterval, numericIntIntervalToValue) (
    using StandardIntervalIntegerOps
  )
  with IntOps[Int, Value]:
  
  override def integerValue(v: Value): Option[Int] = v match
    case IntervalIntV(iv) if iv.isConstant => Some(iv.low)
    case _ => None

private class IntervalIntVOrderingOps(using except: Except[BaseIRException, ?, ?])
  extends LiftedOrderingOps[Value, Topped[Boolean], IntInterval, Topped[Boolean]](valueAsNumericIntInterval, identity)

private class IntervalDoubleVOrderingOps(using except: Except[BaseIRException, ?, ?])
  extends LiftedOrderingOps[Value, Topped[Boolean], DoubleInterval, Topped[Boolean]](valueAsNumericDoubleInterval, identity)


trait IntervalWidenV extends BaseWidenV:
  var intBounds: Set[Int] = Set()
  var doubleBounds: Set[Double] = Set()
  lazy val intIntervalWiden = new NumericIntervalWiden[Int](intBounds, Integer.MIN_VALUE, Integer.MAX_VALUE)
  lazy val doubleIntervalWiden = new NumericIntervalWiden[Double](doubleBounds, Double.MinValue, Double.MaxValue)

  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) => IntervalIntV(intIntervalWiden.apply(iv1, iv2).get)
    case (IntervalDoubleV(iv1), IntervalDoubleV(iv2)) => IntervalDoubleV(doubleIntervalWiden.apply(iv1, iv2).get)
    case _ => super.combine(lhs, rhs)

trait IntervalJoinV extends BaseJoinV:
  def joinInterval[T](v1: NumericInterval[T], v2: NumericInterval[T])(using ord: Ordering[T]): NumericInterval[T] =
    NumericInterval.safe(ord.min(v1.low, v2.low), ord.max(v1.high, v2.high))

  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) => IntervalIntV(joinInterval(iv1, iv2))
    case (IntervalDoubleV(iv1), IntervalDoubleV(iv2)) => IntervalDoubleV(joinInterval(iv1, iv2))
    case _ => super.combine(lhs, rhs)

trait IntervalMeetV extends BaseMeetV:
  def meetInterval[T](v1: NumericInterval[T], v2: NumericInterval[T])(using ord: Ordering[T]): Option[NumericInterval[T]] =
    val newLow = ord.max(v1.low, v2.low)
    val newHigh = ord.min(v1.high, v2.high)
    if (ord.lteq(newLow, newHigh))
      Some(NumericInterval.safe(newLow, newHigh))
    else
      None  // No intersection

  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) => meetInterval(iv1, iv2)
      .map(IntervalIntV.apply)
      .getOrElse(throwBotException())
    case (IntervalDoubleV(iv1), IntervalDoubleV(iv2)) => meetInterval(iv1, iv2)
      .map(IntervalDoubleV.apply)
      .getOrElse(throwBotException())
    case _ => super.meet(lhs, rhs)


class IntervalArithmeticRefinementOps extends ArithmeticRefinementOps[Value]:
  def refineInt(iv1: IntInterval, iv2: IntInterval, op: BinaryArithmeticComparisonOperator): (IntInterval, IntInterval) =
    def safe(a: Int, b: Int) = NumericInterval.safe(a, b)

    val (l1, h1) = (iv1.low, iv1.high)
    val (l2, h2) = (iv2.low, iv2.high)
    op match
      case BinaryArithmeticComparisonOperator.Geq =>
        (safe(math.max(l1, l2), h1), safe(l2, math.min(h2, h1)))
      case BinaryArithmeticComparisonOperator.Gt =>
        (safe(math.max(l1, l2 + 1), h1), safe(l2, math.min(h2, h1 - 1)))
      case BinaryArithmeticComparisonOperator.Leq =>
        (safe(l1, math.min(h1, h2)), safe(math.max(l2, l1), h2))
      case BinaryArithmeticComparisonOperator.Lt =>
        (safe(l1, math.min(h1, h2 - 1)), safe(math.max(l2, l1 + 1), h2))

  def refineDouble(iv1: DoubleInterval, iv2: DoubleInterval, op: BinaryArithmeticComparisonOperator): (DoubleInterval, DoubleInterval) =
    def safe(a: Double, b: Double) = NumericInterval.safe(a, b)

    val (l1, h1) = (iv1.low, iv1.high)
    val (l2, h2) = (iv2.low, iv2.high)
    op match
      case BinaryArithmeticComparisonOperator.Geq =>
        (safe(math.max(l1, l2), h1), safe(l2, math.min(h2, h1)))
      case BinaryArithmeticComparisonOperator.Gt =>
        (safe(math.max(l1, math.nextUp(l2)), h1), safe(l2, math.min(h2, math.nextDown(h1))))
      case BinaryArithmeticComparisonOperator.Leq =>
        (safe(l1, math.min(h1, h2)), safe(math.max(l2, l1), h2))
      case BinaryArithmeticComparisonOperator.Lt =>
        (safe(l1, math.min(h1, math.nextDown(h2))), safe(math.max(l2, math.nextUp(l1)), h2))


  override def refine(v1: Value, v2: Value, op: BinaryArithmeticComparisonOperator): (Value, Value) = (v1, v2) match
    case (IntervalIntV(i1), IntervalIntV(i2)) =>
      val (refinedV1, refinedV2) = refineInt(i1, i2, op)
      (IntervalIntV(refinedV1), IntervalIntV(refinedV2))
    case (IntervalDoubleV(i1), IntervalDoubleV(i2)) =>
      val (refinedV1, refinedV2) = refineDouble(i1, i2, op)
      (IntervalDoubleV(refinedV1), IntervalDoubleV(refinedV2))
    case (Value.Top, _) | (_, Value.Top) =>
      (Value.Top, Value.Top)
    case _ =>
      throw IllegalArgumentException(s"Can not refine non-interval values! $v1 :: $v2")


trait IntervalAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value] = IntervalIntVOps(using failure, effects, except)
  val doubleOps: FloatOps[Double, Value] = IntervalDoubleVOps(using failure, effects, except)
  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = IntervalIntVOrderingOps(using except)
  val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = IntervalDoubleVOrderingOps(using except)
  val arithmeticRefinementOps: ArithmeticRefinementOps[Value] = IntervalArithmeticRefinementOps()
