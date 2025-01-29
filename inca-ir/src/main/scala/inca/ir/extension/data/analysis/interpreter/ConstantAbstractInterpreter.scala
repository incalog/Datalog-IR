package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, ConstantRelation, Value}
import inca.ir.analysis.constant.ConstantInterpreter
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Join, Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.data.{JoinMap, CombineEquiSeq}
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class ConstantDataV(caseDef: CaseDefinitionReference, args: Seq[Value]) extends Value:
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"
  override def isConstant: Boolean = args.forall(_.isConstant)

case class ConstantDataSet(cases: Map[CaseDefinitionReference, Seq[Value]]) extends Value:
  override def toString: String =
    val strings = cases.map { (caseDef, args) =>
      s"${caseDef.name}${args.mkString("(", ",", ")")}"
    }
    strings.mkString("{", ", ", "}")
  override def isConstant: Boolean = cases.size == 1 && cases.head._2.forall(_.isConstant)

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if c1 != c2 =>
      Topped.Actual(false)
    case (ConstantDataV(_, args1), ConstantDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(true)) { case (matches, (a1, a2)) =>
        boolOps.and(matches, this.equ(a1, a2))
      }
    case (ConstantDataSet(cases1), ConstantDataSet(cases2)) =>
      if (cases1.keySet.intersect(cases2.keySet).isEmpty)
        Topped.Actual(false)
      else if (cases1.size == 1 && cases2.size == 2)
        cases1.head._2.zip(cases2.head._2).foldLeft(Topped.Actual(true)) { case (matches, (a1, a2)) =>
          boolOps.and(matches, this.equ(a1, a2))
        }
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) if (c1 != c2) =>
      Topped.Actual(true)
    case (ConstantDataV(_, args1), ConstantDataV(_, args2)) =>
      args1.zip(args2).foldLeft(Topped.Actual(false)) { case (matches, (a1, a2)) =>
        boolOps.or(matches, this.neq(a1, a2))
    }
    case (ConstantDataSet(cases1), ConstantDataSet(cases2)) =>
      if (cases1.keySet.intersect(cases2.keySet).isEmpty)
        Topped.Actual(true)
      else if (cases1.size == 1 && cases2.size == 2)
        cases1.head._2.zip(cases2.head._2).foldLeft(Topped.Actual(false)) { case (matches, (a1, a2)) =>
          boolOps.or(matches, this.equ(a1, a2))
        }
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  var count = 0
  given Join[Value] = this
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(c1, args1), ConstantDataV(c2, args2)) =>
      if (c1 == c2)
        ConstantDataV(c1, args1.zip(args2).map(join))
      else
        ConstantDataSet(Map(c1 -> args1, c2 -> args2))
    case (ConstantDataV(c1, args1), ConstantDataSet(cases2)) =>
      cases2.get(c1) match
        case None => ConstantDataSet(cases2 + (c1 -> args1))
        case Some(args2) => ConstantDataSet(cases2 + (c1 -> Join(args2, args1).get))
    case (ConstantDataSet(cases1), ConstantDataV(c2, args2)) =>
      cases1.get(c2) match
        case None => ConstantDataSet(cases1 + (c2 -> args2))
        case Some(args1) => ConstantDataSet(cases1 + (c2 -> Join(args1, args2).get))
    case (ConstantDataSet(cases1), ConstantDataSet(cases2)) =>
      if (count > 20)
        Value.Top
      else {
        count += 1
        try ConstantDataSet(Join(cases1, cases2).get)
        finally count -= 1
      }
    case _ => super.join(lhs, rhs)

/*trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantDataV(d1, c1, args1), ConstantDataV(d2, c2, args2)) if (d1 == d2) && (c1 == c2) =>
      ConstantDataV(d1, c1, args1.zip(args2).map(meet(_, _)))
    case _ => super.meet(lhs, rhs)*/

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]
  with ConstantInterpreter:
  
  case class ConstructorKind(cd: CaseDefinitionReference) extends ValueKind

  override def getValueKind(v: Value): ValueKind = v match
    case ConstantDataV(cd, _) => ConstructorKind(cd)
    case _ => super.getValueKind(v)

  val dataOps: DataOps[Value, ConstantRelation] = new DataOps[Value, ConstantRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      ConstantDataV(caseDef, args)

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => ConstantRelation)(notMatching: => ConstantRelation): ConstantRelation = v match
      case ConstantDataV(`caseDef`, cArgs) =>
        matching(cArgs)
      case ConstantDataV(_, _) =>
        // caseDef or dataDef do not match
        notMatching
      case ConstantDataSet(cases) =>
        cases.get(caseDef) match
          case None => notMatching
          case Some(args) => matching(args)
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }
