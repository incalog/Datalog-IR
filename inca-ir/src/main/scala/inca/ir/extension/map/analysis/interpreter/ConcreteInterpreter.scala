package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin

case class CMapV(ts: Map[Value, Value]) extends Value:
  override def toString: String = s"Set${ts.mkString("(", ",", ")")}"
  override def isConstant: Boolean = ts.forall((k, v) => k.isConstant && v.isConstant)

private class CMapVOps extends MapOps[Value, Boolean]:
  override def mapLit(vs: Seq[(Value, Value)]): Value = CMapV(vs.toMap)

  override def contains(s: Value, mem: Value): Boolean = s match
    case CMapV(ts) => ts.contains(mem)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def union(ts: Seq[Value]): Value =
    val newVs = ts.foldLeft(Map[Value, Value]()) {
      case (acc, CMapV(ts)) => acc ++ ts
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }
    CMapV(newVs)

  override def iter(s: Value): Iterable[(Value, Value)] = s match
    case CMapV(ts) => ts
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val mapOps: MapOps[Value, Boolean] = CMapVOps()
