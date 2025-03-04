package inca.ir.extension.map.analysis.interpreter

import inca.ir.Param
import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import sturdy.data.MayJoin.NoJoin

trait CMapVBase extends Value


case class CMapV(ts: Map[Value, Set[Value]]) extends CMapVBase:
  override def toString: String = s"Map${ts.toSeq.mkString("(", ",", ")")}"
  override def isConstant: Boolean = ts.forall((k, v) => k.isConstant && v.forall(_.isConstant))

// We only ever allow keys that are where inputs at some point.
// That is due to the demand transformation.
case class CMapFunV(knownKeys: Set[Value], f: Value => Set[Value]) extends CMapVBase:
  override def toString: String = s"MapFun()"
  override def isConstant: Boolean = false


private class CMapVOps extends MapOps[Value, Boolean]:
  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    CMapV(values)

  override def mapFun(f: Value => Set[Value]): Value = CMapFunV(Set(), f)

  override def contains(m: Value, key: Value): Boolean = m match
    case CMapFunV(knownKeys, f) => knownKeys.contains(key)
    case CMapV(ts) => ts.contains(key)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(ts: Seq[Value]): Value =
    ts.foldLeft[CMapVBase](CMapV(Map())) {
      case (CMapV(ts1), CMapV(ts2)) =>
        val newMap = (ts1.keys ++ ts2.keys).map { key =>
          (ts1.get(key), ts2.get(key)) match
            case (Some(v1), Some(v2)) => key -> (v1 ++ v2)
            case (None, Some(v)) => key -> v
            case (Some(v), None) => key -> v
            case _ => throw IllegalStateException()
        }.toMap
        CMapV(newMap)
      case (CMapV(ts1), CMapFunV(knownKeys, f)) =>
        CMapFunV(knownKeys ++ ts1.keys, { key =>
          ts1.get(key) match
            case Some(values) => values ++ f(key)
            case _ if knownKeys.contains(key) => f(key)
            case _ => Set()
        })
      case (CMapFunV(knownKeys, f), CMapV(ts1)) =>
        CMapFunV(knownKeys ++ ts1.keys, { key =>
          ts1.get(key) match
            case Some(values) => values ++ f(key)
            case _ if knownKeys.contains(key) => f(key)
            case _ => Set()
        })
      case (CMapFunV(knownKeys1, f1), CMapFunV(knownKeys2, f2)) =>
        CMapFunV(knownKeys1 ++ knownKeys2, { key => f1(key) ++ f2(key) })
      case (_, m) =>
        throw IllegalStateException(s"Expected map but got $m")
    }

  override def concat(m1: Value, m2: Value): Value = (m1, m2) match
    // only retain m1(k) = v if k not in m2
    case (CMapV(ts1), CMapV(ts2)) => CMapV(ts1 ++ ts2)
    case (CMapV(ts1), CMapFunV(knownKeys, f)) =>
      CMapFunV(knownKeys ++ ts1.keys, { key =>
        if (knownKeys.contains(key))
          f(key)
        else
          ts1.getOrElse(key, Set())
      })
    case (CMapFunV(knownKeys, f), CMapV(ts1)) =>
      CMapFunV(knownKeys ++ ts1.keys, { key =>
        if (ts1.contains(key))
          ts1(key)
        else if (knownKeys.contains(key))
          f(key)
        else
          Set()
      })
    case (CMapFunV(knownKeys1, f1), CMapFunV(knownKeys2, f2)) =>
      CMapFunV(knownKeys1 ++ knownKeys2, { key =>
        if (knownKeys2.contains(key))
          f2(key)
        else if (knownKeys1.contains(key))
          f1(key)
        else
          Set()
      })
    case (_, m) =>
      throw IllegalStateException(s"Expected map but got $m")

  override def plus(m: Value, k: Value, v: Value): Value = m match
    case CMapFunV(knownKeys, f) => CMapFunV(knownKeys, key => if (key == k) Set(v) else f(key))
    case CMapV(ts) => CMapV(ts + (k -> Set(v)))

  override def lookup(m: Value, k: Value): Seq[Value] = m match
    case CMapFunV(knownKeys, f) => f(k).toSeq
    case CMapV(ts) => ts.getOrElse(k, Set()).toSeq
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value): Iterable[Value] = m match
    case CMapFunV(knownKeys, _) => knownKeys
    case CMapV(ts) => ts.keys
    case _ => throw IllegalArgumentException(s"Expected map but got $m")


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val mapOps: MapOps[Value, Boolean] = CMapVOps()
