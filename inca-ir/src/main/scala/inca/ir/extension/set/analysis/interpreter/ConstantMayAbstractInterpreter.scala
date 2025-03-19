package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.values.Topped.Actual
import sturdy.values.ordering.EqOps
import sturdy.values.{Powerset, Topped}


case class ConstantMaySetV private(var values: Set[Value]) extends Value:
  override def toString: String = s"Set${values.mkString("(", ",", ")")}"
  // Since elements may be contained in a set, we can never know for sure that a set is constant, except when it's empty
  override def isConstant: Boolean = values.isEmpty

  def union(other: ConstantMaySetV): ConstantMaySetV =
    ConstantMaySetV(values.union(other.values))

  def intersect(other: ConstantMaySetV): ConstantMaySetV = (this, other) match
    case (ConstantMaySetV.top, _) => other
    case (_, ConstantMaySetV.top) => this
    case _ => ConstantMaySetV(values.intersect(other.values))

object ConstantMaySetV:
  val empty: ConstantMaySetV = new ConstantMaySetV(Set())
  // Note: ConstantMaySetV.top != Top, since ConstantMaySetV.top is definitely not the empty set
  val top: ConstantMaySetV = new ConstantMaySetV(Set(Value.Top))

  def apply(values: Value*): ConstantMaySetV = apply(values.toSet)

  // normalize the set
  def apply(values: Set[Value]): ConstantMaySetV =
    if (values.contains(Value.Top))
      ConstantMaySetV.top
    else
      new ConstantMaySetV(values)

// This Constant analysis approximates elements that may be contained in a set.
private class ConstantMaySetVOps(using eqOps: EqOps[Value, Topped[Boolean]]) extends SetOps[Value, Topped[Boolean]]:
  override def setLit(vs: Seq[Value]): Value = ConstantMaySetV(vs.toSet)

  override def contains(s: Value, mem: Value): Topped[Boolean] = s match
    case Value.Top => Topped.Top
    case ConstantMaySetV(values) =>
      val notContained = values.forall(eqOps.equ(_, mem) == Topped.Actual(false))
      if (notContained)
        Topped.Actual(false)
      else
        Topped.Top
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def isEmpty(s: Value): Topped[Boolean] = s match
    case Value.Top => Topped.Top
    case ConstantMaySetV(vs) => Topped.Actual(vs.isEmpty)

  override def union(sets: Seq[Value]): Value =
    sets.foldLeft[Value](ConstantMaySetV.empty) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (acc: ConstantMaySetV, s: ConstantMaySetV) => acc.union(s)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }

  override def intersect(sets: Seq[Value]): Value =
      sets.foldLeft[Value](ConstantMaySetV.top) {
        case (Value.Top, _) | (_, Value.Top) => Value.Top
        case (acc: ConstantMaySetV, s: ConstantMaySetV) => acc.intersect(s)
        case (_, s) => throw IllegalStateException(s"Expected set but got $s")
      }

  override def iter(s: Value): Iterable[Value] = s match
    case Value.Top => Seq(Value.Top)
    case s: ConstantMaySetV => s.values
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConstantMayEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (s1: ConstantMaySetV, s2: ConstantMaySetV) =>
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
    case (s1: ConstantMaySetV, s2: ConstantMaySetV) =>
      val allElementsAreEqual = (s1.values.size == s2.values.size) && s1.values.forall(v => s2.values.exists(neq(v, _) == Topped.Actual(false)))
      val atLeastOneDisjointElement = s1.values.exists(v => s2.values.forall(neq(v, _) == Topped.Actual(true)))
      if (allElementsAreEqual)
        Topped.Actual(false)
      else if (atLeastOneDisjointElement)
        Topped.Actual(true)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantMayJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantMaySetV, s2: ConstantMaySetV) => s1.union(s2)
    case _ => super.join(lhs, rhs)

trait ConstantMayMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantMaySetV, s2: ConstantMaySetV) => s1.intersect(s2)
    case _ => super.meet(lhs, rhs)

trait ConstantMayAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override val setOps: SetOps[Value, Topped[Boolean]] = ConstantMaySetVOps(using eqOps)
