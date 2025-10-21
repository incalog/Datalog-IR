package inca.ir.extension.arithmetic.analysis.interpreter

import inca.ir
import inca.ir.Term
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, BaseWidenV, FiniteAbstractRelation, Value}
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

val topIntInterval: IntInterval = NumericInterval.safe(scala.Int.MinValue, scala.Int.MaxValue)
val topDoubleInterval: DoubleInterval = NumericInterval.safe(Double.MinValue, Double.MaxValue)

val edbNumericUpperBound = 5000 //scala.Int.MaxValue - 2

case class IntervalIntV(private val iv: IntInterval) extends Value:
  override def toString: String = iv.toString
  override def isConstant: Boolean = iv.isConstant
  override def isFinite: Boolean =
    if (iv.low == scala.Int.MinValue)
      false
    else if (iv.high == scala.Int.MaxValue)
      false
    else
      true

object IntervalIntV:
  def constant(i: Int): IntervalIntV = new IntervalIntV(NumericInterval.constant(i))
  def finite: IntervalIntV = new IntervalIntV(NumericInterval.safe(0, edbNumericUpperBound))
  def finite(l: Int, u: Int): IntervalIntV = new IntervalIntV(NumericInterval.safe(l, u))
  def apply(iv: IntInterval): Value =
    if (iv == topIntInterval)
      Value.Top
    else
      new IntervalIntV(iv)

case class IntervalDoubleV(iv: DoubleInterval) extends Value:
  override def toString: String = iv.toString
  override def isConstant: Boolean = iv.isConstant
  override def isFinite: Boolean =
    if (iv.low == scala.Double.MinValue)
      false
    else if (iv.high == scala.Double.MaxValue)
      false
    else
      true

object IntervalDoubleV:
  def constant(d: Double): IntervalDoubleV = new IntervalDoubleV(NumericInterval.constant(d))
  def finite: IntervalDoubleV = new IntervalDoubleV(NumericInterval.safe(0, edbNumericUpperBound))

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

private def numericIntIntervalToValue(value: IntInterval): Value = value match
  case iv if iv == topIntInterval => Value.Top
  case iv => IntervalIntV(iv)

private def valueAsNumericIntInterval(v: Value)(using except: Except[BaseIRException, ?, ?]): IntInterval = v match
  case IntervalIntV(iv) => iv
  case Value.Top => topIntInterval
  case _ => throw IllegalArgumentException(s"Can not convert $v to int")


trait IntervalWidenV extends BaseWidenV:
  var intBounds: Set[Int] = Set()
  var doubleBounds: Set[Double] = Set()
  lazy val intIntervalWiden = new NumericIntervalWiden[Int](intBounds + (edbNumericUpperBound + 1) + (edbNumericUpperBound - 1), scala.Int.MinValue, scala.Int.MaxValue)
  lazy val doubleIntervalWiden = new NumericIntervalWiden[Double](doubleBounds, Double.MinValue, Double.MaxValue)

  // TODO: We should also widen, once the interval is bigger than Y, e.g. 10.000
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (IntervalIntV(iv1), IntervalIntV(iv2)) =>
      //val iv1InIv2 = iv1.low >= iv2.low && iv1.high <= iv2.high
      //val iv2InIv1 = iv2.low >= iv1.low && iv2.high <= iv1.high
      //if (iv1InIv2) IntervalIntV(intIntervalWiden.apply(iv2, iv1).get)
      //else IntervalIntV(intIntervalWiden.apply(iv1, iv2).get)
      IntervalIntV(intIntervalWiden.apply(iv1, iv2).get)
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
    case (Value.Top, IntervalIntV(i2)) =>
      val (refinedV1, refinedV2) = refineInt(topIntInterval, i2, op)
      (IntervalIntV(refinedV1), IntervalIntV(refinedV2))
    case (IntervalIntV(i1), Value.Top) =>
      val (refinedV1, refinedV2) = refineInt(i1, topIntInterval, op)
      (IntervalIntV(refinedV1), IntervalIntV(refinedV2))
    case (Value.Top, IntervalDoubleV(i2)) =>
      val (refinedV1, refinedV2) = refineDouble(topDoubleInterval, i2, op)
      (IntervalDoubleV(refinedV1), IntervalDoubleV(refinedV2))
    case (IntervalDoubleV(i1), Value.Top) =>
      val (refinedV1, refinedV2) = refineDouble(i1, topDoubleInterval, op)
      (IntervalDoubleV(refinedV1), IntervalDoubleV(refinedV2))
    case (Value.Top, Value.Top) =>
      (Value.Top, Value.Top)
    case _ =>
      throw IllegalArgumentException(s"Can not refine non-interval values! $v1 :: $v2")

trait FiniteIntOps[I, V] extends IntOps[I, V]:
  def interval(l: I, h: I): V

trait IntervalAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: FiniteIntOps[Int, Value] = new LiftedIntegerOps[Int, Value, IntInterval](valueAsNumericIntInterval(_)(using except), numericIntIntervalToValue) (
      using StandardIntervalIntegerOps
    )
    with IntOps[Int, Value]
    with FiniteIntOps[Int, Value]:

    override def integerValue(v: Value): Option[Int] = v match
      case IntervalIntV(iv) if iv.isConstant => Some(iv.low)
      case _ => None

    override def interval(l: Int, h: Int): Value =
      IntervalIntV(NumericInterval.safe(l, h))

  val doubleOps: FloatOps[Double, Value] = LiftedFloatOps[Double, Value, DoubleInterval](valueAsNumericDoubleInterval(_)(using except), numericDoubleIntervalToValue) (
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


  val intOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], IntInterval, Topped[Boolean]](valueAsNumericIntInterval(_)(using except), identity)

  val doubleOrderingOps: OrderingOps[Value, Topped[Boolean]] = new LiftedOrderingOps[Value, Topped[Boolean], DoubleInterval, Topped[Boolean]](valueAsNumericDoubleInterval(_)(using except), identity)

  val arithmeticRefinementOps: ArithmeticRefinementOps[Value] = IntervalArithmeticRefinementOps()
