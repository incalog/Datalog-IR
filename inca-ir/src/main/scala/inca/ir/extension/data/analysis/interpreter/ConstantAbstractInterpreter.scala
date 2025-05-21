package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference}
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
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantDataV(caseDef: CaseDefinitionReference, args: Seq[Value]) extends Value:
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"
  override def isConstant: Boolean = args.forall(_.isConstant)

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if c1 != c2 =>
      Topped.Actual(false)
    case (ConstantDataV(_, args1), ConstantDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(true)) { case (matches, (a1, a2)) =>
        boolOps.and(matches, this.equ(a1, a2))
      }
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if (c1 != c2) =>
      Topped.Actual(true)
    case (ConstantDataV(_, args1), ConstantDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(false)) { case (matches, (a1, a2)) =>
        boolOps.or(matches, this.neq(a1, a2))
    }
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if (c1 == c2) =>
      ConstantDataV(c1, args1.zip(args2).map(combine(_, _)))
    case _ => super.combine(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if c1 == c2 =>
      ConstantDataV(c1, args1.zip(args2).map(meet(_, _)))
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, AbstractRelation] = new DataOps[Value, AbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      ConstantDataV(caseDef, args)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => AbstractRelation)(notMatching: => AbstractRelation): AbstractRelation = v match
      case ConstantDataV(`caseDef`, cArgs) =>
        matching(cArgs)
      case ConstantDataV(_, _) =>
        // caseDef or dataDef do not match
        notMatching
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }

    override def deconstructNeg(v: Value, caseDef: CaseDefinitionReference)(possibleSuccess: Seq[Value] => AbstractRelation)(success: => AbstractRelation): AbstractRelation = v match
      case ConstantDataV(`caseDef`, cArgs) => possibleSuccess(cArgs)
      case ConstantDataV(_, _) => success
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          possibleSuccess(caseDef.args.map(_ => Value.Top))
        } {
          success
        }