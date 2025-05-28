package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, FiniteAbstractRelation, Value}
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

// TODO: For now this is just a constant analysis

case class FiniteDataV(caseDef: CaseDefinitionReference, args: Seq[Value]) extends Value:
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"
  override def isConstant: Boolean = args.forall(_.isConstant)
  override def isFinite: Boolean = args.forall(_.isFinite)

trait FiniteEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (FiniteDataV(c1, args1), FiniteDataV(c2, args2)) if c1 != c2 =>
      Topped.Actual(false)
    case (FiniteDataV(_, args1), FiniteDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(true)) { case (matches, (a1, a2)) =>
        boolOps.and(matches, this.equ(a1, a2))
      }
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (FiniteDataV(c1, args1), FiniteDataV(c2, args2)) if (c1 != c2) =>
      Topped.Actual(true)
    case (FiniteDataV(_, args1), FiniteDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(false)) { case (matches, (a1, a2)) =>
        boolOps.or(matches, this.neq(a1, a2))
    }
    case _ => super.neq(v1, v2)

trait FiniteJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (FiniteDataV(c1, args1), FiniteDataV(c2, args2)) if (c1 == c2) =>
      FiniteDataV(c1, args1.zip(args2).map(combine(_, _)))
    case _ => super.combine(lhs, rhs)

trait FiniteMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (FiniteDataV(c1, args1), FiniteDataV(c2, args2)) if c1 == c2 =>
      FiniteDataV(c1, args1.zip(args2).map(meet(_, _)))
    case _ => super.meet(lhs, rhs)

trait FiniteAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, FiniteAbstractRelation] = new DataOps[Value, FiniteAbstractRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      FiniteDataV(caseDef, args)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => FiniteAbstractRelation)(notMatching: => FiniteAbstractRelation): FiniteAbstractRelation = v match
      case FiniteDataV(`caseDef`, cArgs) =>
        matching(cArgs)
      case FiniteDataV(_, _) =>
        // caseDef or dataDef do not match
        notMatching
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }

    override def deconstructNeg(v: Value, caseDef: CaseDefinitionReference)(possibleSuccess: Seq[Value] => FiniteAbstractRelation)(success: => FiniteAbstractRelation): FiniteAbstractRelation = v match
      case FiniteDataV(`caseDef`, cArgs) => possibleSuccess(cArgs)
      case FiniteDataV(_, _) => success
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          possibleSuccess(caseDef.args.map(_ => Value.Top))
        } {
          success
        }