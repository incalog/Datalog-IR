package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin


case class CMapV(ts: Map[Value, Set[Value]]) extends Value:
  override def toString: String = s"Set${ts.mkString("(", ",", ")")}"
  override def isConstant: Boolean = ts.forall((k, v) => k.isConstant && v.forall(_.isConstant))

private class CMapVOps extends MapOps[Value, Boolean]:
  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    CMapV(values)

  override def contains(m: Value, mem: Value): Boolean = m match
    case CMapV(ts) => ts.contains(mem)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(ts: Seq[Value]): Value =
    val newVs = ts.foldLeft(Map[Value, Set[Value]]()) {
      case (acc, CMapV(ts)) => ??? // We can produce a multimap here I assume
      case (_, m) => throw IllegalStateException(s"Expected map but got $m")
    }
    CMapV(newVs)

  override def concat(m1: Value, m2: Value): Value = ???
    // only retain m1(k) = v if k not in m2
    /*val newVs = ts.foldLeft(Map[Value, Value]()) {
      case (acc, CMapV(ts)) => acc ++ ts
      case (_, m) => throw IllegalStateException(s"Expected map but got $m")
    }
    CMapV(newVs)*/

  override def plus(m: Value, k: Value, v: Value): Value = ???

  override def lookup(m: Value, k: Value): Seq[Value] = m match
    case CMapV(ts) if ts.contains(k) => ts(k).toSeq
    case CMapV(ts) => throw IllegalArgumentException(s"Key $k not found in map $ts")
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value): Iterable[Value] = m match
    case CMapV(ts) => ts.keys
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val mapOps: MapOps[Value, Boolean] = CMapVOps()
