package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import inca.ir.extension.data.CaseDefinitionReference
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.data.WithJoin

case class DataKindV(caseDefs: Set[CaseDefinitionReference]) extends Value:
  override def toString: String =
    val caseStr = caseDefs.map { c =>
      val argS = c.args.map(_ => "?").mkString("(", ",", ")")
      s"${c.name}$argS"
    }
    caseStr.mkString("{", ",", "}")
  override def isConstant: Boolean = caseDefs.size == 1 && caseDefs.head.args.isEmpty

trait DataKindEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataKindV(caseDefs1), DataKindV(caseDefs2)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (DataKindV(caseDefs1), DataKindV(caseDefs2)) =>
      val intersection = caseDefs1.intersect(caseDefs2)
      if (intersection.isEmpty)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait DataKindJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataKindV(caseDefs1), DataKindV(caseDefs2)) =>
      DataKindV(caseDefs1.union(caseDefs2))
    case _ => super.join(lhs, rhs)

trait DataKindMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (DataKindV(caseDefs1), DataKindV(caseDefs2)) =>
      val intersect = caseDefs1.intersect(caseDefs2)
      if (intersect.isEmpty)
        throwBotException()
      else
        DataKindV(intersect)
    case _ => super.meet(lhs, rhs)

trait DataKindAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:

  val dataOps: DataOps[Value, ConstantRelation] = new DataOps[Value, ConstantRelation]:
    override def construct(caseDef: CaseDefinitionReference, args: Seq[Value]): Value =
      DataKindV(Set(caseDef))

    override def deconstruct(v: Value, caseDef: CaseDefinitionReference)(matching: Seq[Value] => ConstantRelation)(notMatching: => ConstantRelation): ConstantRelation = v match
      case DataKindV(caseDefs) =>
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
