package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import inca.ir.extension.set.analysis.interpreter.GenericInterpreter
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.values.{Powerset, Topped}

case class ConstantSetV(ts: Set[Value]) extends Value:
  override def toString: String = s"Set${ts.mkString("(", ",", ")")}"
  override def isConstant: Boolean = ts.forall(_.isConstant)

// This Constant analysis approximates elements that may be contained in a set.
private class ConstantSetVOps extends SetOps[Value, Topped[Boolean]]:
  override def setLit(vs: Seq[Value]): Value = ConstantSetV(vs.toSet)

  override def contains(s: Value, mem: Value): Topped[Boolean] = s match
    case ConstantSetV(ts) =>
      if (ts.contains(mem))
        Topped.Top // May be contained
      else
        Topped.Actual(false) // definitely not contained
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def intersect(ts: Seq[Value]): Value =
    val init = union(ts).asInstanceOf[ConstantSetV].ts
    val newVs = ts.foldLeft(init) {
      case (acc, ConstantSetV(ts)) => acc.intersect(ts)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }
    ConstantSetV(newVs)

  override def union(ts: Seq[Value]): Value =
    val newVs = ts.foldLeft(Set[Value]()) {
      case (acc, ConstantSetV(ts)) => acc.union(ts)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }
    ConstantSetV(newVs)

  override def iter(s: Value): Iterable[Value] = s match
    case ConstantSetV(ts) => ts
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantSetV(s1), ConstantSetV(s2)) => Topped.Actual(s1 == s2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (ConstantSetV(s1), ConstantSetV(s2)) => Topped.Actual(s1 != s2)
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantSetV(s1), ConstantSetV(s2)) => ConstantSetV(s1.union(s2))
    case _ => super.join(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (ConstantSetV(s1), ConstantSetV(s2)) => ConstantSetV(s1.intersect(s2))
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override val setOps: SetOps[Value, Topped[Boolean]] = ConstantSetVOps()
