package inca.ir.extension.map.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, ConstantRelation, Meet, RequireJoin, RequireMeet, Value}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.EffectStack
import sturdy.effect.except.Except
import sturdy.values.booleans.BooleanOps
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Powerset, Topped}


enum BoundedMapV extends Value:
  case Empty // Either the map is empty or no value exists
  case NonEmpty(keyBound: Value, valueBound: Value)

  override def toString: String = this match
    case Empty => s"⌈ε -> ε⌉"
    case NonEmpty(keyBound, valueBound) => s"⌈$keyBound -> $valueBound⌉"

  override def isConstant: Boolean = false

  def key: Value = this match
    case BoundedMapV.NonEmpty(k, _) => k
    case _ => throw IllegalStateException("Empty map has no key")

  def value: Value = this match
    case BoundedMapV.NonEmpty(_, v) => v
    case _ => throw IllegalStateException("Empty map has no value")

object BoundedMapV:
  val empty: BoundedMapV = BoundedMapV.Empty
  val Top: BoundedMapV = BoundedMapV(Value.Top -> Value.Top)

  def apply(bound: (Value, Value)): BoundedMapV.NonEmpty = BoundedMapV.NonEmpty(bound._1, bound._2)
  def apply(keyBound: Value, valueBound: Value): BoundedMapV.NonEmpty = BoundedMapV.NonEmpty(keyBound, valueBound)

private class BoundedMapVOps(using eqOps: EqOps[Value, Topped[Boolean]],
                             effects: EffectStack,
                             except: Except[BaseIRException, Powerset[BaseIRException], WithJoin],
                             joinV: Join[Value], meetV: Meet[Value],
                             withJoinV: WithJoin[Value], joinRV: Join[ConstantRelation])
  extends MapOps[Value, ConstantRelation, Topped[Boolean]]:

  override def mapLit(vs: Seq[(Value, Value)]): Value =
    if (vs.isEmpty)
      BoundedMapV.Empty
    else
      val keyBound = vs.map(_._1).reduce(joinV(_, _).get)
      val valueBound = vs.map(_._2).reduce(joinV(_, _).get)
      BoundedMapV(keyBound -> valueBound)

  // We don't analyse map functions for now
  override def mapFun(f: Value => Set[Value]): Value = BoundedMapV.Top

  override def contains(m: Value, key: Value): Topped[Boolean] = m match
    case Value.Top => Topped.Top
    case BoundedMapV.Empty => Topped.Actual(false)
    case BoundedMapV.NonEmpty(keyBound, _) =>
      if (joinV(keyBound, key).get == keyBound)
        // `keyBound` is an upper bound for `key`, that means the value could be contained
        Topped.Top
      else
        Topped.Actual(false)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def union(maps: Seq[Value]): Value =
    maps.foldLeft[Value](BoundedMapV.Empty) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (BoundedMapV.Empty, m) => m
      case (m, BoundedMapV.Empty) => m
      case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) =>
        val keyBound = joinV(k1, k2).get
        val valueBound = joinV(v1, v2).get
        BoundedMapV(keyBound -> valueBound)
      case (_, v2) => throw IllegalStateException(s"Expected map but got $v2")
    }

  override def concat(m1: Value, m2: Value): Value = (m1, m2) match
    // only retain m1(k) = v if k not in m2
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case (BoundedMapV.Empty, m) => m
    case (m, BoundedMapV.Empty) => m
    case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) =>
      // Can we be more precise?
      val keyBound = joinV(k1, k2).get
      val valueBound = joinV(v1, v2).get
      BoundedMapV(keyBound -> valueBound)
    case (m1, m2) => throw IllegalStateException(s"Expected maps but got $m1 and $m2")

  override def lookup(m: Value, key: Value)(foundValues: Set[Value] => ConstantRelation)(noValuesOrKeyNotFound: => ConstantRelation): ConstantRelation = m match
    case Value.Top =>
      effects.joinComputations {
        foundValues(Set(Value.Top))
      } {
        noValuesOrKeyNotFound
      }
    case BoundedMapV.Empty =>
      noValuesOrKeyNotFound
    case BoundedMapV.NonEmpty(keyBound, valueBound) =>
      if (joinV(keyBound, key).get == keyBound)
        // `keyBound` is an upper bound for `key`, that means the value could be contained
        effects.joinComputations {
          foundValues(Set(valueBound))
        } {
          noValuesOrKeyNotFound
        }
      else
        noValuesOrKeyNotFound
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def keyIter(m: Value)(keySet: Set[Value] => ConstantRelation)(noKeys: => ConstantRelation): ConstantRelation = m match
    case Value.Top =>
      effects.joinComputations {
        keySet(Set(Value.Top))
      } {
        noKeys
      }
    case BoundedMapV.Empty =>
      noKeys
    case BoundedMapV.NonEmpty(keyBound, valueBound) =>
      keySet(Set(valueBound))
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

  override def plus(m: Value, k: Value, v: Value): Value = m match
    case Value.Top => Value.Top
    case BoundedMapV.Empty => BoundedMapV(k -> v)
    case BoundedMapV.NonEmpty(oldKeyBound, oldValueBound) =>
      val keyBound = joinV(oldKeyBound, k).get
      val valueBound = joinV(oldValueBound, v).get
      BoundedMapV(keyBound -> valueBound)
    case _ => throw IllegalArgumentException(s"Expected map but got $m")

trait BoundedEqOps(using boolOps: BooleanOps[Topped[Boolean]]) extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (BoundedMapV.Empty, BoundedMapV.Empty) => Topped.Actual(true)
    case (BoundedMapV.Empty, _: BoundedMapV.NonEmpty) => Topped.Actual(false)
    case (_: BoundedMapV.NonEmpty, BoundedMapV.Empty) => Topped.Actual(false)
    case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) =>
      boolOps.and(equ(k1, k2), equ(v1, v2))
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (BoundedMapV.Empty, BoundedMapV.Empty) => Topped.Actual(false)
    case (BoundedMapV.Empty, _: BoundedMapV.NonEmpty) => Topped.Actual(true)
    case (_: BoundedMapV.NonEmpty, BoundedMapV.Empty) => Topped.Actual(true)
    case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) =>
      boolOps.or(neq(k1, k2), neq(v1, v2))
    case _ => super.neq(v1, v2)

trait BoundedJoinV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseJoinV:
  override def join(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (BoundedMapV.Empty, _) => rhs
    case (_, BoundedMapV.Empty) => lhs
    case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) => BoundedMapV(join(k1, k2) -> join(v1, v2))
    case _ => super.join(lhs, rhs)

trait BoundedMeetV[J[_] <: MayJoin[?]](using eqOps: EqOps[Value, Topped[Boolean]], except: Except[BaseIRException, ?, J], mayJoinV: J[Value]) extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (BoundedMapV.Empty, _) => BoundedMapV.Empty
    case (_, BoundedMapV.Empty) => BoundedMapV.Empty
    case (BoundedMapV.NonEmpty(k1, v1), BoundedMapV.NonEmpty(k2, v2)) =>
      except.tryCatch {
        BoundedMapV(meet(k1, k2) -> meet(v1, v2))
      } /* catch */ { exec =>
        // Meet produced Bot
        BoundedMapV.Empty
      }(using mayJoinV)
    case _ => super.meet(lhs, rhs)

trait BoundedAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], ConstantRelation, Powerset[BaseIRException], WithJoin]
  with RequireMeet[Value]
  with RequireJoin[Value]:

  override lazy val mapOps: MapOps[Value, ConstantRelation, Topped[Boolean]] = BoundedMapVOps(using eqOps, effects, except, joinV, meetV, mayJoinV, joinRV)
