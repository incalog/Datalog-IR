package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
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
import sturdy.values.Topped.Top
import sturdy.values.integer.given_OrderingOps_Int_Boolean

case class DataShapeV(caseDefs: Set[CaseDefinitionReference]) extends Value:
  override def toString: String =
    val caseStr = caseDefs.map { c =>
      val argS = c.args.map(_ => "?").mkString("(", ",", ")")
      s"${c.name}$argS"
    }
    caseStr.mkString("{", ",", "}")
  override def isConstant: Boolean = caseDefs.size == 1 && caseDefs.head.args.isEmpty

trait ShapeEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataShapeV(caseDefs1), DataShapeV(caseDefs2)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataShapeV(caseDefs1), DataShapeV(caseDefs2)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait ShapeJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataShapeV(caseDefs1), DataShapeV(caseDefs2)) =>
      DataShapeV(caseDefs1.union(caseDefs2))
    case _ => super.join(lhs, rhs)

trait ShapeMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataShapeV(caseDefs1), DataShapeV(caseDefs2)) =>
      DataShapeV(caseDefs1.intersect(caseDefs2))
    case _ => super.meet(lhs, rhs)

trait ShapeAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, ConstantRelation] = new DataOps[Value, ConstantRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      DataShapeV(Set(caseDef))

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => ConstantRelation)(notMatching: => ConstantRelation): ConstantRelation = v match
      case DataShapeV(caseDefs) =>
        if (caseDefs.contains(caseDef))
          effects.joinComputations {
            matching(caseDef.args.map(_ => Value.Top))
          } {
            notMatching
          }
        else
          notMatching
      case Value.Top =>
        // Could or could not match
        effects.joinComputations {
          matching(caseDef.args.map(_ => Value.Top))
        } {
          notMatching
        }
