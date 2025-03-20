package inca.ir.extension.bool.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Value}
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.data.WithJoin

case class ConstantBoolV(bool: Boolean) extends Value:
  override def toString: String = bool.toString
  override def isConstant: Boolean = true

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantBoolV(b1), ConstantBoolV(b2)) => Topped.Actual(b1 == b2)
    case _ => super.equ(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantBoolV(b1), ConstantBoolV(b2)) if b1 == b2 => ConstantBoolV(b1)
    case _ => super.join(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantBoolV(b1), ConstantBoolV(b2)) if b1 == b2 => ConstantBoolV(b1)
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  val booleanOps: BooleanOps[Value] = new BooleanOps[Value]:

    override def boolLit(b: Boolean): Value = ConstantBoolV(b)

    override def and(v1: Value, v2: Value): Value = (v1, v2) match
      case (_, ConstantBoolV(false)) | (ConstantBoolV(false), _) => ConstantBoolV(false)
      case (ConstantBoolV(b1), ConstantBoolV(b2)) => ConstantBoolV(b1 && b2)
      case (_, Value.Top) | (Value.Top, _) => Value.Top
      case _ => failure(InvalidBooleanOp, s"Can not apply logical and between $v1 and $v2")

    override def or(v1: Value, v2: Value): Value = (v1, v2) match
      case (_, ConstantBoolV(true)) | (ConstantBoolV(true), _) => ConstantBoolV(true)
      case (ConstantBoolV(b1), ConstantBoolV(b2)) => ConstantBoolV(b1 || b2)
      case (_, Value.Top) | (Value.Top, _) => Value.Top
      case _ => failure(InvalidBooleanOp, s"Can not apply logical or between $v1 and $v2")

    override def not(v: Value): Value = v match
      case ConstantBoolV(b) => ConstantBoolV(!b)
      case Value.Top => Value.Top
      case _ => failure(InvalidBooleanOp, s"Can not negate $v")