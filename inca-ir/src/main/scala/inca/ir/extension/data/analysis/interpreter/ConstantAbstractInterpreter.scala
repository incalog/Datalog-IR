package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, ConstantRelation, Top, Value}
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

case class ConstantDataV(dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference, args: Seq[Value]) extends Value:
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(d1, c1, args1), ConstantDataV(d2, c2, args2)) if (d1 != d2) || (c1 != c2) =>
      Topped.Actual(false)
    case (ConstantDataV(_, _, args1), ConstantDataV(_, _, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(true)) { case (matches, (a1, a2)) =>
        boolOps.and(matches, super.equ(a1, a2))
      }
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(d1, c1, args1), ConstantDataV(d2, c2, args2)) if (d1 != d2) || (c1 != c2) =>
      Topped.Actual(true)
    case (ConstantDataV(_, _, args1), ConstantDataV(_, _, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(false)) { case (matches, (a1, a2)) =>
        boolOps.or(matches, super.neq(a1, a2))
    }
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(d1, c1, args1), ConstantDataV(d2, c2, args2)) if (d1 == d2) && (c1 == c2) =>
      ConstantDataV(d1, c1, args1.zip(args2).map(join(_, _)))
    case _ => super.join(lhs, rhs)

/*trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(d1, c1, args1), ConstantDataV(d2, c2, args2)) if (d1 == d2) && (c1 == c2) =>
      ConstantDataV(d1, c1, args1.zip(args2).map(meet(_, _)))
    case _ => super.meet(lhs, rhs)*/

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  val dataOps: DataOps[Value, ConstantRelation] = new DataOps[Value, ConstantRelation]:
    override def construct(dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      ConstantDataV(dataDef, caseDef, args)

    override def deconstruct(v: Value, dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference)(matching: Seq[Value] => ConstantRelation)(notMatching: => ConstantRelation): ConstantRelation = v match
      case ConstantDataV(`dataDef`, `caseDef`, cArgs) =>
        matching(cArgs)
      case ConstantDataV(_, _, _) =>
        // caseDef or dataDef do not match
        notMatching
      case Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Top))
        } {
          notMatching
        }
