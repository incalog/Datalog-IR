package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Value}
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.EffectStack
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, Powerset, Topped}


case class ConstantSetV private(var values: Set[Value]) extends Value:
  override def toString: String = s"Set${values.mkString("(", ",", ")")}"
  override def isConstant: Boolean = values.forall(_.isConstant)

  def union(other: ConstantSetV): ConstantSetV =
    ConstantSetV(values.union(other.values))

  def intersect(other: ConstantSetV): ConstantSetV = (this, other) match
    case (ConstantSetV.Top, _) => other
    case (_, ConstantSetV.Top) => this
    case _ => ConstantSetV(values.intersect(other.values))

object ConstantSetV:
  val empty: ConstantSetV = new ConstantSetV(Set())
  // Note: ConstantSetV.Top != Top, since ConstantSetV.Top is definitely not the empty set
  val Top: ConstantSetV = new ConstantSetV(Set(Value.Top))

  def apply(values: Value*): ConstantSetV = apply(values.toSet)

  // normalize the set
  def apply(values: Set[Value]): ConstantSetV =
    if (values.contains(Value.Top))
      ConstantSetV.Top
    else
      new ConstantSetV(values)

private class ConstantSetVOps(using eqOps: EqOps[Value, Topped[Boolean]], effects: EffectStack, joinRV: Join[AbstractRelation]) extends SetOps[Value, AbstractRelation, Topped[Boolean]]:
  override def setLit(vs: Seq[Value]): Value = ConstantSetV(vs.toSet)

  override def contains(s: Value, mem: Value): Topped[Boolean] = s match
    case Value.Top => Topped.Top
    case ConstantSetV(values) =>
      val contained = values.exists(eqOps.equ(_, mem) == Topped.Actual(true))
      val notContained = values.forall(eqOps.equ(_, mem) == Topped.Actual(false))
      if (contained)
        Topped.Actual(true)
      else if (notContained)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def union(sets: Seq[Value]): Value =
    sets.foldLeft[Value](ConstantSetV.empty) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (acc: ConstantSetV, s: ConstantSetV) => acc.union(s)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }

  override def intersect(sets: Seq[Value]): Value =
      sets.foldLeft[Value](ConstantSetV.Top) {
        case (Value.Top, _) | (_, Value.Top) => Value.Top
        case (acc: ConstantSetV, s: ConstantSetV) => acc.intersect(s)
        case (_, s) => throw IllegalStateException(s"Expected set but got $s")
      }

  override def iter(s: Value)(values: Set[Value] => AbstractRelation)(empty: => AbstractRelation): AbstractRelation = s match
    case Value.Top =>
      effects.joinComputations {
        values(Set(Value.Top))
      } {
        empty
      }(using joinRV)
    case ConstantSetV(vs) if vs.isEmpty =>
      empty
    case ConstantSetV(vs) =>
      values(vs)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (s1: ConstantSetV, s2: ConstantSetV) =>
      val allElementsAreEqual = (s1.values.size == s2.values.size) && s1.values.forall(v => s2.values.exists(equ(v, _) == Topped.Actual(true)))
      val atLeastOneDisjointElement = s1.values.exists(v => s2.values.forall(equ(v, _) == Topped.Actual(false)))
      if (allElementsAreEqual)
        Topped.Actual(true)
      else if (atLeastOneDisjointElement)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (s1: ConstantSetV, s2: ConstantSetV) =>
      val allElementsAreEqual = (s1.values.size == s2.values.size) && s1.values.forall(v => s2.values.exists(neq(v, _) == Topped.Actual(false)))
      val atLeastOneDisjointElement = s1.values.exists(v => s2.values.forall(neq(v, _) == Topped.Actual(true)))
      if (allElementsAreEqual)
        Topped.Actual(false)
      else if (atLeastOneDisjointElement)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantJoinV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantSetV, s2: ConstantSetV) =>
      val sameSet = eqOps.equ(s1, s2)
      if (sameSet.isActual && sameSet.get)
        s1
      else
        Value.Top
    case _ => super.combine(lhs, rhs)

trait ConstantMeetV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantSetV, s2: ConstantSetV) =>
      val sameSet = eqOps.equ(s1, s2)
      if (sameSet.isActual && sameSet.get)
        s1
      else
        Value.Top
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  lazy val setOps: SetOps[Value, AbstractRelation, Topped[Boolean]] = ConstantSetVOps(using eqOps, effects, joinRV)
