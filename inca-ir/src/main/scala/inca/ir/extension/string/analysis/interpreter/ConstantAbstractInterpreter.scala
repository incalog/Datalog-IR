package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.IntOps
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantStringV(value: String) extends Value:
  override def toString: String = s"\"$value\""
  override def isConstant: Boolean = true

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantStringV(s1), ConstantStringV(s2)) => Topped.Actual(s1 == s2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantStringV(s1), ConstantStringV(s2)) => Topped.Actual(s1 != s2)
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantStringV(s1), ConstantStringV(s2)) if s1 == s2 => lhs
    case _ => super.combine(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantStringV(s1), ConstantStringV(s2)) if s1 == s2 => lhs
    case _ => super.meet(lhs, rhs)

class ConstantStringVOps(using failure: Failure, except: Except[BaseIRException, ?, ?], intOps: IntOps[Int, Value]) extends StringOps[Topped[Boolean], Value]:
  override def stringLit(s: String): Value = ConstantStringV(s)

  override def toString(v: Value): Value = v match
    case Value.Top => Value.Top
    case _: ConstantStringV => v
    case _ => ConstantStringV(v.toString)

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (ConstantStringV(s1), ConstantStringV(s2)) => ConstantStringV(s1 ++ s2)
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

  override def substring(v: Value, index: Value, length: Value): Value = v match
    case Value.Top => Value.Top
    case ConstantStringV(s) =>
      val idx = intOps.integerValue(index)
      val len = intOps.integerValue(length)
      (idx, len) match
        case (Some(i), Some(l)) => ConstantStringV(s.substring(i, i+l))
        case _ => Value.Top
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def stringLength(v: Value): Value = v match
    case Value.Top => Value.Top
    case ConstantStringV(s) => intOps.integerLit(s.length)
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def ordinalNumber(v: Value): Value = v match
    case Value.Top => Value.Top
    case ConstantStringV(s) => intOps.integerLit(s.hashCode)
    case _ => failure(InvalidStringValue, s"Can not get ordinal number of $v")

  override def matches(v: Value, pattern: Value): Topped[Boolean] = (v, pattern) match
    case (Value.Top, _) | (_, Value.Top) => Topped.Top
    case (ConstantStringV(s), ConstantStringV(p)) => Topped.Actual(p.r.matches(s))
    case _ => failure(InvalidStringConcat, s"Can not regex match values $v and $pattern")

  override def stringValue(v: Value): String = v match
    case ConstantStringV(value) => value
    case _ => failure(InvalidStringValue, s"Value $v has no string value")


trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value]
  lazy val stringOps: StringOps[Topped[Boolean], Value] = ConstantStringVOps(using failure, except, intOps)
