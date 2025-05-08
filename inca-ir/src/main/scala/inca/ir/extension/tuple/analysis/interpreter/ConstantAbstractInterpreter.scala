package inca.ir.extension.tuple.analysis.interpreter

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

import scala.annotation.targetName

object ConstantTupleV:
  @targetName("createConstantTupleV")
  def apply(ts: Value*): ConstantTupleV = new ConstantTupleV(ts)

case class ConstantTupleV(ts: Seq[Value]) extends Value:
  override def toString: String = ts.mkString("(", ",", ")")
  override def isConstant: Boolean = ts.forall(_.isConstant)

trait ConstantEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantTupleV(ts1), ConstantTupleV(ts2)) =>
      ts1.zip(ts2).foldLeft(Topped.Actual(true)) { case (matches, (t1, t2)) =>
        boolOps.and(matches, this.equ(t1, t2))
      }
    case _ => super.equ(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantTupleV(ts1), ConstantTupleV(ts2)) => ConstantTupleV(ts1.zip(ts2).map(combine(_, _)))
    case _ => super.combine(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantTupleV(ts1), ConstantTupleV(ts2)) => ConstantTupleV(ts1.zip(ts2).map(meet(_, _)))
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:

  val tupleOps: TupleOps[Value] = new TupleOps[Value]:
    override def tupleLit(ts: Seq[Value]): Value = ConstantTupleV(ts)

    override def project(t: Value, index: Int): Value = t match
      case Value.Top => Value.Top
      case ConstantTupleV(ts) if index >= 0 && (index < ts.size) => ts(index)
      case ConstantTupleV(_) => failure(InvalidTupleProjection, s"Index $index out of bounds")
      case _ => failure(InvalidTupleProjection, s"Expected a tuple, but got $t")

    override def iter(v: Value): Seq[Value] = v match
      case ConstantTupleV(ts) => ts //ts.flatMap(iter)
      case _ => Seq(v)