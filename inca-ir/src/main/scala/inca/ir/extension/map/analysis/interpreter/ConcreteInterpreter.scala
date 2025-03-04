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
case class CMapFunV(f: Value => Set[Value]) extends CMapVBase:
  override def toString: String = s"MapFun()"
  override def isConstant: Boolean = false


private class CMapVOps extends MapOps[Value, Boolean]:
  // TODO: We could keep track of all keys that are demanded for a MapFunV here
  //  That way, we could fill the map at the end and also implement iter

  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    CMapV(values)

  override def mapFun(f: Value => Set[Value]): Value = CMapFunV(f)

  override def contains(m: Value, key: Value): Boolean = m match
    case fun@CMapFunV(f) => f(key).nonEmpty
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
      case (CMapV(ts1), CMapFunV(f)) =>
        CMapFunV { key =>
          ts1.get(key) match
            case Some(values) => values ++ f(key)
            case _ => f(key)
        }
      case (CMapFunV(f), CMapV(ts1)) =>
        CMapFunV { key =>
          ts1.get(key) match
            case Some(values) => values ++ f(key)
            case _ => f(key)
        }
      case (CMapFunV(f1), CMapFunV(f2)) =>
        CMapFunV { key => f1(key) ++ f2(key) }
      case (_, m) =>
        throw IllegalStateException(s"Expected map but got $m")
    }

  override def concat(m1: Value, m2: Value): Value = (m1, m2) match
    // only retain m1(k) = v if k not in m2
    case (CMapV(ts1), CMapV(ts2)) => CMapV(ts1 ++ ts2)
    case (CMapV(ts1), CMapFunV(f)) =>
      CMapFunV { key =>
        val res = f(key)
        if (res.nonEmpty)
          res
        else
          ts1.getOrElse(key, Set())
      }
    case (CMapFunV(f), CMapV(ts1)) =>
      CMapFunV { key =>
        if (ts1.contains(key))
          ts1(key)
        else
          f(key)
      }
    case (CMapFunV(f1), CMapFunV(f2)) =>
      CMapFunV { key =>
        val res = f2(key)
        if (res.nonEmpty)
          res
        else
          f1(key)
      }
    case (_, m) =>
      throw IllegalStateException(s"Expected map but got $m")

  override def plus(m: Value, k: Value, v: Value): Value = m match
    case CMapFunV(f) => CMapFunV { key => if (key == k) Set(v) else f(key) }
    case CMapV(ts) => CMapV(ts + (k -> Set(v)))

  override def lookup(m: Value, k: Value): Seq[Value] = m match
    case fun@CMapFunV(f) => f(k).toSeq
    case CMapV(ts) => ts.getOrElse(k, Set()).toSeq
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value): Iterable[Value] = m match
    case CMapFunV(_) => throw UnsupportedOperationException("Can not iterate keys of MapFun")
    case CMapV(ts) => ts.keys
    case _ => throw IllegalArgumentException(s"Expected map but got $m")


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  override val mapOps: MapOps[Value, Boolean] = CMapVOps()
