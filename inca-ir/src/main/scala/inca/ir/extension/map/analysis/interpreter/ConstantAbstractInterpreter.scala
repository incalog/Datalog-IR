package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import inca.ir.extension.map.analysis.interpreter.GenericInterpreter
import sturdy.data.MayJoin.WithJoin
import sturdy.values.Topped.Top
import sturdy.values.{Powerset, Topped}

trait ConstantMapVBase extends Value:
  def union(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.top
  def intersect(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.top
  def concat(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.top
  def contains(key: Value): Topped[Boolean] = Topped.Top
  def plus(k: Value, v: Value): ConstantMapVBase = ConstantMapV.top
  def lookup(k: Value): Seq[Value] = Seq(Value.Top)
  def keyIter: Iterable[Value] = Seq(Value.Top)

object ConstantMapV:
  val empty: ConstantMapV = ConstantMapV(Map())
  val top: ConstantMapV = ConstantMapV(Map(Value.Top -> Set(Value.Top)))

  // Normalise the map to correctly handle top values
  def apply(data: Map[Value, Set[Value]]): ConstantMapV =
      if (data.contains(Value.Top))
        new ConstantMapV(Map(Value.Top -> Set(Value.Top)))
      else
        new ConstantMapV(data.map { (key, vs) =>
          if (vs.contains(Value.Top)) key -> Set(Value.Top)
          else key -> vs
        })

  def apply(kv: (Value, Set[Value])*): ConstantMapV = new ConstantMapV(kv.toMap)

case class ConstantMapV private (var data: Map[Value, Set[Value]]) extends ConstantMapVBase:
  override def toString: String = s"Map${data.toSeq.mkString("(", ",", ")")}"
  // Since elements may be contained in a map, we can never know for sure that a map is constant
  override def isConstant: Boolean = false

  override def contains(key: Value): Topped[Boolean] =
    if (data.nonEmpty && key == Value.Top) Topped.Top
    else if (data.contains(Value.Top)) Topped.Top // Could contain anything
    else if (data.contains(key)) Topped.Top // May be contained
    else Topped.Actual(false)  // definitely not contained

  override def lookup(k: Value): Seq[Value] =
    if (data.nonEmpty && k == Value.Top) Seq(Value.Top)
    else if (data.contains(Value.Top)) Seq(Value.Top) // a top key subsumes all other keys
    else data.getOrElse(k, Set()).toSeq

  override def keyIter: Iterable[Value] = data.keys

  override def plus(k: Value, v: Value): ConstantMapVBase = ConstantMapV(data + (k -> Set(v)))

  override def union(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMapV(data2) =>
      val newMap = (data.keys ++ data2.keys).map { key =>
        (data.get(key), data2.get(key)) match
          case (Some(v1), Some(v2)) => key -> (v1 ++ v2)
          case (None, Some(v)) => key -> v
          case (Some(v), None) => key -> v
          case _ => throw IllegalStateException()
      }.toMap
      ConstantMapV(newMap)
    case _: ConstantMapFunV =>
      ConstantMapV.top

  override def concat(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMapV(data2) => ConstantMapV(data ++ data2)
    case _: ConstantMapFunV => ConstantMapV.top

  override def intersect(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMapV(data2) =>
      if (data.contains(Value.Top) || data2.contains(Value.Top))
        ConstantMapV.top
      else
        val newMap = data.keySet.intersect(data2.keySet).map { key =>
          (data(key), data2(key)) match
            case (Value.Top, _) | (Value.Top, _) => key -> Set(Value.Top)
            case (v1, v2) => key -> v1.intersect(v2)
        }.toMap
        ConstantMapV(newMap)
    case _: ConstantMapFunV =>
      ConstantMapV.top


// We don't analyse map funs for now
case class ConstantMapFunV(f: Value => Set[Value]) extends ConstantMapVBase:
  override def toString: String = s"MapFun()"
  override def isConstant: Boolean = false


// This Constant analysis approximates elements that may be contained in a map.
private class ConstantMapVOps extends MapOps[Value, Topped[Boolean]]:

  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    ConstantMapV(values)

  override def mapFun(f: Value => Set[Value]): Value = ConstantMapFunV(f)

  override def contains(m: Value, key: Value): Topped[Boolean] = m match
    case map: ConstantMapVBase => map.contains(key)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(maps: Seq[Value]): Value =
    if (maps.contains(Value.Top))
      Value.Top
    else
      maps.foldLeft[ConstantMapVBase](ConstantMapV.empty) {
        case (acc, m: ConstantMapVBase) => acc.union(m)
        case (acc, v) => throw IllegalArgumentException(s"Expected map but got $v")
      }

  override def concat(m1: Value, m2: Value): Value = (m1, m2) match
    // only retain m1(k) = v if k not in m2
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case (map1: ConstantMapVBase, map2: ConstantMapVBase) => map1.concat(map2)
    case _ => throw IllegalArgumentException(s"Expected maps but got $m1 and $m2")

  override def plus(m: Value, k: Value, v: Value): Value = m match
    case Value.Top => Value.Top
    case map: ConstantMapVBase => map.plus(k, v)
    case _ => throw IllegalArgumentException(s"Expected maps but got $m")

  override def lookup(m: Value, k: Value): Seq[Value] = m match
    case Value.Top => Seq(Value.Top)
    case map: ConstantMapVBase => map.lookup(k)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value): Iterable[Value] = m match
    case Value.Top => Seq(Value.Top)
    case map: ConstantMapVBase => map.keyIter
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (m1: ConstantMapV, m2: ConstantMapV) => Topped.Actual(m1.data == m2.data)
    case (_: ConstantMapFunV, _) | (_, _: ConstantMapFunV) => Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (m1: ConstantMapV, m2: ConstantMapV) => Topped.Actual(m1.data != m2.data)
    case (_: ConstantMapFunV, _) | (_, _: ConstantMapFunV) => Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) => m1.union(m2)
    case _ => super.join(lhs, rhs)

trait ConstantMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) => m1.intersect(m2)
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override val mapOps: MapOps[Value, Topped[Boolean]] = ConstantMapVOps()
