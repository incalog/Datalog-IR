package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Value}
import sturdy.data.MayJoin.WithJoin
import sturdy.effect.EffectStack
import sturdy.values.Topped.Top
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, Powerset, Topped}

private trait ConstantMapVBase extends Value:
  def union(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.Top
  def intersect(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.Top
  def concat(other: ConstantMapVBase): ConstantMapVBase = ConstantMapV.Top
  def contains(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] = Topped.Top
  def lookup(k: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Set[Value] = Set(Value.Top)
  def plus(k: Value, v: Value): ConstantMapVBase = ConstantMapV.Top
  def isEmpty: Topped[Boolean] = Topped.Top
  def keySet: Set[Value] = Set(Value.Top)

object ConstantMapV:
  val empty: ConstantMapV = new ConstantMapV(Map())
  val Top: ConstantMapV = new ConstantMapV(Map(Value.Top -> Set(Value.Top)))

  // Normalise the map to correctly handle Top values
  def apply(data: Map[Value, Set[Value]]): ConstantMapV =
      if (data.contains(Value.Top))
        ConstantMapV.Top
      else
        new ConstantMapV(data.map { (key, vs) =>
          if (vs.contains(Value.Top))
            key -> Set(Value.Top)
          else
            key -> vs
        })

  def apply(kv: (Value, Set[Value])*): ConstantMapV = new ConstantMapV(kv.toMap)

case class ConstantMapV private(var data: Map[Value, Set[Value]]) extends ConstantMapVBase:
  override def toString: String = s"Map${data.toSeq.mkString("(", ",", ")")}"

  override def isConstant: Boolean = data.isEmpty

  override def isEmpty: Topped[Boolean] = Topped.Actual(data.isEmpty)

  override def contains(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Topped[Boolean] =
    val contained = data.keys.exists(eqOps.equ(_, key) == Topped.Actual(true))
    val notContained = data.keys.forall(eqOps.equ(_, key) == Topped.Actual(false))
    if (contained)
      Topped.Actual(true)
    else if (notContained)
      Topped.Actual(false)
    else
      Topped.Top

  override def lookup(key: Value)(using eqOps: EqOps[Value, Topped[Boolean]]): Set[Value] =
    val contained = data.keys.exists(eqOps.equ(_, key) == Topped.Actual(true))
    val containsTop = data.keys.exists(_ == Value.Top)
    if (contained)
      data(key)
    else if (data.nonEmpty && (key == Value.Top))
      // We have no explicit Top key
      Set(Value.Top)
    else if (containsTop)
      // We request a key not explicitly contained in the map, but we have a Top key in the map
      data(Value.Top)
    else
      // We don't find the key at all
      Set[Value]()

  override def keySet: Set[Value] = data.keySet

  override def plus(k: Value, v: Value): ConstantMapVBase = ConstantMapV(data + (k -> Set(v)))

  override def union(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMapV(data2) =>
      val relevantKeys = data.keys ++ data2.keys
      val newMap = relevantKeys.map { key =>
        (data.get(key), data2.get(key)) match
          case (Some(v1), Some(v2)) => key -> (v1.union(v2))
          case (None, Some(v)) => key -> v
          case (Some(v), None) => key -> v
          case _ => throw IllegalStateException()
      }.toMap
      ConstantMapV(newMap)
    case _: ConstantMapFunV =>
      ConstantMapV.Top

  override def concat(other: ConstantMapVBase): ConstantMapVBase = other match
    case ConstantMapV(data2) => ConstantMapV(data ++ data2)
    case _: ConstantMapFunV => ConstantMapV.Top

  override def intersect(other: ConstantMapVBase): ConstantMapVBase = (this, other) match
    case (ConstantMapV.Top, _) => other
    case (_, ConstantMapV.Top) => this
    case (ConstantMapV(data), ConstantMapV(data2)) =>
      // We know that we don't have a Top key here
      val relevantKeys = data.keySet.intersect(data2.keySet)
      val newMap = relevantKeys.map { key =>
        (data(key), data2(key)) match
          case (s1, s2) if s1.contains(Value.Top) => key -> s2
          case (s1, s2) if s2.contains(Value.Top) => key -> s1
          case (s1, s2) => key -> s1.intersect(s2)
      }.toMap
      ConstantMapV(newMap)
    case (_, _: ConstantMapFunV) =>
      ConstantMapV.Top


// We don't analyse map funs for now
case class ConstantMapFunV(f: Value => Set[Value]) extends ConstantMapVBase:
  override def toString: String = s"MapFun()"
  override def isConstant: Boolean = false


// This Constant analysis approximates elements that may be contained in a map.
private class ConstantMapVOps(using eqOps: EqOps[Value, Topped[Boolean]], effects: EffectStack, joinRV: Join[ConstantRelation]) extends MapOps[Value, ConstantRelation, Topped[Boolean]]:

  override def mapLit(vs: Seq[(Value, Value)]): Value =
    val values = vs.groupBy(_._1).map { (k, kv) => k -> kv.map(_._2).toSet }
    ConstantMapV(values)

  override def mapFun(f: Value => Set[Value]): Value = ConstantMapFunV(f)

  override def contains(m: Value, key: Value): Topped[Boolean] = m match
    case map: ConstantMapVBase => map.contains(key)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(maps: Seq[Value]): Value =
    maps.foldLeft[Value](ConstantMapV.empty) {
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

  override def lookup(m: Value, k: Value)(foundValues: Set[Value] => ConstantRelation)(noValuesOrKeyNotFound: => ConstantRelation): ConstantRelation = m match
    case Value.Top =>
      effects.joinComputations {
        foundValues(Set(Value.Top))
      } {
        noValuesOrKeyNotFound
      }(using joinRV)
    case map: ConstantMapVBase =>
      map.contains(k) match
        case Topped.Actual(false) => noValuesOrKeyNotFound
        case Topped.Actual(true) =>
          val vs = map.lookup(k)
          if (vs.isEmpty)
            noValuesOrKeyNotFound
          else
            foundValues(vs)
        case Topped.Top =>
          effects.joinComputations {
            val vs = map.lookup(k)
            assert(vs.nonEmpty)
            foundValues(vs)
          } {
            noValuesOrKeyNotFound
          }(using joinRV)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value)(keySet: Set[Value] => ConstantRelation)(noKeys: => ConstantRelation): ConstantRelation = m match
    case Value.Top =>
      effects.joinComputations {
        keySet(Set(Value.Top))
      } {
        noKeys
      }(using joinRV)
    case map: ConstantMapVBase =>
      map.isEmpty match
        case Topped.Actual(true) => noKeys
        case Topped.Actual(false) => keySet(map.keySet)
        case Topped.Top =>
          effects.joinComputations {
            keySet(map.keySet)
          } {
            noKeys
          }(using joinRV)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

trait ConstantEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] =
    def iterablesAreEqual(i1: Iterable[Value], i2: Iterable[Value]): Boolean =
      (i1.size == i2.size) && i1.forall(v => i2.exists(equ(v, _) == Topped.Actual(true)))

    def iterablesAreNotEqual(i1: Iterable[Value], i2: Iterable[Value]) =
      i1.exists(v => i2.forall(equ(v, _) == Topped.Actual(false)))

    (v1, v2) match
      case (m1: ConstantMapV, m2: ConstantMapV) =>
        val allKeysAreEqual = iterablesAreEqual(m1.data.keys, m2.data.keys)
        val atLeastOneDisjointKey = iterablesAreNotEqual(m1.data.keys, m2.data.keys)
        if (allKeysAreEqual)
          val allValuesAreEqual = m1.data.keys.forall { k => iterablesAreEqual(m1.data(k), m2.data(k)) }
          val atLeastOneDisjointValue = m1.data.keys.forall { k => iterablesAreEqual(m1.data(k), m2.data(k)) }
          if (allValuesAreEqual)
            Topped.Actual(true)
          else if (atLeastOneDisjointValue)
            Topped.Actual(false)
          else
            Topped.Top
        else if (atLeastOneDisjointKey)
          Topped.Actual(false)
        else
          Topped.Top
      case (_: ConstantMapFunV, _) | (_, _: ConstantMapFunV) => Topped.Top
      case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (m1: ConstantMapV, m2: ConstantMapV) =>
      def iterablesAreEqual(i1: Iterable[Value], i2: Iterable[Value]): Boolean =
        (i1.size == i2.size) && i1.forall(v => i2.exists(neq(v, _) == Topped.Actual(false)))

      def iterablesAreNotEqual(i1: Iterable[Value], i2: Iterable[Value]) =
        i1.exists(v => i2.forall(neq(v, _) == Topped.Actual(true)))

      (v1, v2) match
        case (m1: ConstantMapV, m2: ConstantMapV) =>
          val allKeysAreEqual = iterablesAreEqual(m1.data.keys, m2.data.keys)
          val atLeastOneDisjointKey = iterablesAreNotEqual(m1.data.keys, m2.data.keys)
          if (allKeysAreEqual)
            val allValuesAreEqual = m1.data.keys.forall { k => iterablesAreEqual(m1.data(k), m2.data(k)) }
            val atLeastOneDisjointValue = m1.data.keys.forall { k => iterablesAreEqual(m1.data(k), m2.data(k)) }
            if (allValuesAreEqual)
              Topped.Actual(false)
            else if (atLeastOneDisjointValue)
              Topped.Actual(true)
            else
              Topped.Top
          else if (atLeastOneDisjointKey)
            Topped.Actual(true)
          else
            Topped.Top
    case (_: ConstantMapFunV, _) | (_, _: ConstantMapFunV) => Topped.Top
    case _ => super.neq(v1, v2)

trait ConstantJoinV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) =>
      val sameMap = eqOps.equ(m1, m2)
      if (sameMap.isActual && sameMap.get)
        m1
      else
        Value.Top
    case _ => super.join(lhs, rhs)

trait ConstantMeetV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (m1: ConstantMapVBase, m2: ConstantMapVBase) =>
      val sameMap = eqOps.equ(m1, m2)
      if (sameMap.isActual && sameMap.get)
        m1
      else
        Value.Top
    case _ => super.meet(lhs, rhs)

trait ConstantAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]:
  override lazy val mapOps: MapOps[Value, ConstantRelation, Topped[Boolean]] = ConstantMapVOps(using eqOps, effects, joinRV)
