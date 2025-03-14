package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import inca.ir.extension.set.analysis.interpreter.GenericInterpreter
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.values.{Powerset, Topped}

case class ConstantSetV private (var values: Set[Value]) extends Value:
  lazy val containsTop: Boolean = values.contains(Value.Top)

  override def toString: String = s"Set${values.mkString("(", ",", ")")}"
  // Since elements may be contained in a set, we can never know for sure that a set is constant
  override def isConstant: Boolean = false

  def contains(mem: Value): Topped[Boolean] =
    if (values.contains(Value.Top)) Topped.Top // Could contain anything
    else if (values.contains(mem)) Topped.Top // May be contained
    else Topped.Actual(false) // definitely not contained

  def union(other: ConstantSetV): ConstantSetV =
    if (containsTop || other.containsTop) ConstantSetV.top
    else ConstantSetV(values.union(other.values))

  def intersect(other: ConstantSetV): ConstantSetV =
    if (containsTop || other.containsTop) ConstantSetV.top
    else ConstantSetV(values.intersect(other.values))

object ConstantSetV:
  val empty: ConstantSetV = ConstantSetV(Set())
  val top: ConstantSetV = ConstantSetV(Set(Value.Top))

  // normalize the set
  def apply(values: Set[Value]): ConstantSetV =
    if (values.contains(Value.Top)) top
    else new ConstantSetV(values)

// This Constant analysis approximates elements that may be contained in a set.
private class ConstantSetVOps extends SetOps[Value, Topped[Boolean]]:
  override def setLit(vs: Seq[Value]): Value = ConstantSetV(vs.toSet)

  override def contains(s: Value, mem: Value): Topped[Boolean] = s match
    case Value.Top => Topped.Top
    case s: ConstantSetV => s.contains(mem)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def union(sets: Seq[Value]): Value =
    if (sets.contains(Value.Top))
      Value.Top
    else
      sets.foldLeft(ConstantSetV.empty) {
        case (acc, s: ConstantSetV) => acc.union(s)
        case (_, s) => throw IllegalStateException(s"Expected set but got $s")
      }

  override def intersect(sets: Seq[Value]): Value =
    if (sets.contains(Value.Top))
      Value.Top
    else
      union(sets) match
        case superset: ConstantSetV =>
          sets.foldLeft(superset) {
            case (acc, s: ConstantSetV) => acc.intersect(s)
            case (_, s) => throw IllegalStateException(s"Expected set but got $s")
          }
        case u => throw IllegalStateException(s"Expected ConstantSetV, but got $u")

  override def iter(s: Value): Iterable[Value] = s match
    case Value.Top => Seq(Value.Top)
    case s: ConstantSetV => s.values
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (s1: ConstantSetV, s2: ConstantSetV) => Topped.Actual(s1.values == s2.values)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (s1: ConstantSetV, s2: ConstantSetV) => Topped.Actual(s1.values != s2.values)
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantSetV, s2: ConstantSetV) => s1.union(s2)
    case _ => super.join(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (s1: ConstantSetV, s2: ConstantSetV) => s1.intersect(s2)
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override val setOps: SetOps[Value, Topped[Boolean]] = ConstantSetVOps()
