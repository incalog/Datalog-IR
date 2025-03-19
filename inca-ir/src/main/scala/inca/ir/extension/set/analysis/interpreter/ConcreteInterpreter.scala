package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin

case class CSetV(ts: Set[Value]) extends Value:
  override def toString: String = s"Set${ts.mkString("(", ",", ")")}"
  override def isConstant: Boolean = ts.forall(_.isConstant)

private class CSetVOps extends SetOps[Value, ConcreteRelation[Value], Boolean]:
  override def setLit(vs: Seq[Value]): Value = CSetV(vs.toSet)

  override def contains(s: Value, mem: Value): Boolean = s match
    case CSetV(ts) => ts.contains(mem)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def intersect(ts: Seq[Value]): Value =
    val init = union(ts).asInstanceOf[CSetV].ts
    val newVs = ts.foldLeft(init) {
      case (acc, CSetV(ts)) => acc.intersect(ts)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }
    CSetV(newVs)

  override def union(ts: Seq[Value]): Value =
    val newVs = ts.foldLeft(Set[Value]()) {
      case (acc, CSetV(ts)) => acc.union(ts)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }
    CSetV(newVs)

  override def iter(s: Value)(values: Set[Value] => ConcreteRelation[Value])(empty: => ConcreteRelation[Value]): ConcreteRelation[Value] = s match
    case CSetV(ts) if ts.isEmpty => empty
    case CSetV(ts) => values(ts)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val setOps: SetOps[Value, ConcreteRelation[Value], Boolean] = CSetVOps()
