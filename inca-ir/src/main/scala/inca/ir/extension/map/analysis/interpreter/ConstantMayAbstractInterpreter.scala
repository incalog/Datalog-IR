package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import sturdy.data.MayJoin.WithJoin
import sturdy.values.ordering.EqOps
import sturdy.values.{Powerset, Topped}

trait ConstantMapVBase extends Value:
  def union(other: ConstantMapVBase): ConstantMapVBase = ConstantMayMapV.top
  def intersect(other: ConstantMapVBase): ConstantMapVBase = ConstantMayMapV.top
  def concat(other: ConstantMapVBase): ConstantMapVBase = ConstantMayMapV.top
  def contains(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] = Topped.Top
  def lookup(k: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Seq[Value] = Seq(Value.Top)
  def plus(k: Value, v: Value): ConstantMapVBase = ConstantMayMapV.top
  def isEmpty: Topped[Boolean] = Topped.Top
  def hasValue(k: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] = Topped.Top
  def keyIter: Iterable[Value] = Seq(Value.Top)

object ConstantMayMapV:
  val empty: ConstantMayMapV = new ConstantMayMapV(Map())
  val top: ConstantMayMapV = new ConstantMayMapV(Map(Value.Top -> Set(Value.Top)))

  // Normalise the map to correctly handle top values
  def apply(data: Map[Value, Set[Value]]): ConstantMayMapV =
      if (data.contains(Value.Top))
        ConstantMayMapV.top
      else
        new ConstantMayMapV(data.map { (key, vs) =>
          if (vs.contains(Value.Top))
            key -> Set(Value.Top)
          else
            key -> vs
        })

  def apply(kv: (Value, Set[Value])*): ConstantMayMapV = new ConstantMayMapV(kv.toMap)

case class ConstantMayMapV private(var data: Map[Value, Set[Value]]) extends ConstantMapVBase:
  override def toString: String = s"Map${data.toSeq.mkString("(", ",", ")")}"
  // Since elements may be contained in a map, we can never know for sure that a map is constant
  override def isConstant: Boolean = data.isEmpty

  override def isEmpty: Topped[Boolean] = Topped.Actual(data.isEmpty)

  override def contains(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] =
    val notContained = data.keys.forall(k => eqOps.equ(k, key) == Topped.Actual(false))
    if (notContained)
      Topped.Actual(false)  // definitely not contained
    else
      Topped.Top

  override def lookup(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Seq[Value] =
    val notContained = data.keys.forall(k => eqOps.equ(k, key) == Topped.Actual(false))
    if (notContained)
      Seq() // definitely not contained
    else if (key == Value.Top)
      Seq(Value.Top)
    else
      // Might be contained
      data.getOrElse(key, Set()).toSeq

  override def keyIter: Iterable[Value] = data.keys

  override def hasValue(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] =
    val notContained = data.keys.forall(k => eqOps.equ(k, key) == Topped.Actual(false))
    if (notContained)
      Topped.Actual(false)
    else if (key == Value.Top)
      Topped.Top
    else
      Topped.Actual(data.getOrElse(key, Set()).nonEmpty)

  override def plus(k: Value, v: Value): ConstantMapVBase = ConstantMayMapV(data + (k -> Set(v)))

  override def union(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMayMapV(data2) =>
      val relevantKeys = data.keys ++ data2.keys
      val newMap = relevantKeys.map { key =>
        (data.get(key), data2.get(key)) match
          case (Some(v1), Some(v2)) => key -> (v1 ++ v2)
          case (None, Some(v)) => key -> v
          case (Some(v), None) => key -> v
          case _ => throw IllegalStateException()
      }.toMap
      ConstantMayMapV(newMap)
    case _: ConstantMayMapFunV =>
      ConstantMayMapV.top

  override def concat(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMayMapV(data2) => ConstantMayMapV(data ++ data2)
    case _: ConstantMayMapFunV => ConstantMayMapV.top

  override def intersect(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMayMapV(data2) =>
        val relevantKeys = data.keySet.intersect(data2.keySet)
        val newMap = relevantKeys.map { key =>
          (data(key), data2(key)) match
            case (v1, v2) => key -> v1.intersect(v2)
        }.toMap
        ConstantMayMapV(newMap)
    case _: ConstantMayMapFunV =>
      ConstantMayMapV.top


// We don't analyse map funs for now
case class ConstantMayMapFunV(f: Value => Set[Value]) extends ConstantMapVBase:
  override def toString: String = s"MapFun()"
  override def isConstant: Boolean = false


// This Constant analysis approximates elements that may be contained in a map.
private class ConstantMayMapVOps(using eqOps: EqOps[Value, Topped[Boolean]]) extends MapOps[Value, Topped[Boolean]]:

  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    ConstantMayMapV(values)

  override def mapFun(f: Value => Set[Value]): Value = ConstantMayMapFunV(f)

  override def isEmpty(m: Value): Topped[Boolean] = m match
    case Value.Top => Topped.Top
    case map: ConstantMapVBase => map.isEmpty
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def contains(m: Value, key: Value): Topped[Boolean] = m match
    case map: ConstantMapVBase => map.contains(key)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(maps: Seq[Value]): Value =
    maps.foldLeft[Value](ConstantMayMapV.empty) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (acc: ConstantMapVBase, m: ConstantMapVBase) => acc.union(m)
      case (_, v) => throw IllegalArgumentException(s"Expected map but got $v")
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

  override def hasValue(m: Value, k: Value): Topped[Boolean] = m match
    case Value.Top => Topped.Top
    case map: ConstantMapVBase => map.hasValue(k)(using eqOps)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (m1: ConstantMayMapV, m2: ConstantMayMapV) => Topped.Actual(m1.data == m2.data)
    case (_: ConstantMayMapFunV, _) | (_, _: ConstantMayMapFunV) => Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (m1: ConstantMayMapV, m2: ConstantMayMapV) => Topped.Actual(m1.data != m2.data)
    case (_: ConstantMayMapFunV, _) | (_, _: ConstantMayMapFunV) => Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantMayJoinV extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) => m1.union(m2)
    case _ => super.join(lhs, rhs)

trait ConstantMayMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) => m1.intersect(m2)
    case _ => super.meet(lhs, rhs)

trait ConstantMayAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override val mapOps: MapOps[Value, Topped[Boolean]] = ConstantMayMapVOps(using eqOps)
