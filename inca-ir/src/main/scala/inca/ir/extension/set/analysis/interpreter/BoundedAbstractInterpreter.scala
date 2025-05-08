package inca.ir.extension.set.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, AbstractRelation, Meet, RequireJoin, RequireMeet, Value}
import sturdy.data.MayJoin
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.EffectStack
import sturdy.effect.except.Except
import sturdy.values.ordering.EqOps
import sturdy.values.{Join, MaybeChanged, Powerset, Topped}


enum BoundedSetV extends Value:
  case Empty
  case NonEmpty(bound: Value)

  override def toString: String = this match
    case Empty => s"⌈ε⌉"
    case NonEmpty(bound) => s"⌈$bound⌉"

  override def isConstant: Boolean = false

object BoundedSetV:
  val Top: BoundedSetV = BoundedSetV.NonEmpty(Value.Top)
  def apply(v: Value): BoundedSetV.NonEmpty = BoundedSetV.NonEmpty(v)

private class BoundedSetVOps(using eqOps: EqOps[Value, Topped[Boolean]],
                             effects: EffectStack,
                             except: Except[BaseIRException, Powerset[BaseIRException], WithJoin],
                             joinV: Join[Value], meetV: Meet[Value],
                             withJoinV: WithJoin[Value],
                             joinRV: Join[AbstractRelation])
  extends SetOps[Value, AbstractRelation, Topped[Boolean]]:

  override def setLit(vs: Seq[Value]): Value =
    if (vs.isEmpty)
      BoundedSetV.Empty
    else
      BoundedSetV.NonEmpty(vs.reduce(joinV(_, _).get))

  override def contains(s: Value, mem: Value): Topped[Boolean] = s match
    case Value.Top => Topped.Top
    case BoundedSetV.Empty => Topped.Actual(false)
    case BoundedSetV.NonEmpty(bound) =>
      if (joinV(bound, mem).get == bound)
        // `bound` is an upper bound for `mem`, that means the value could be contained
        Topped.Top
      else
        Topped.Actual(false)
    case _ => throw IllegalArgumentException(s"Expected set but got $s")

  override def union(sets: Seq[Value]): Value =
    sets.foldLeft[Value](BoundedSetV.Empty) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (BoundedSetV.Empty, s) => s
      case (s, BoundedSetV.Empty) => s
      case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) => BoundedSetV.NonEmpty(joinV(b1, b2).get)
      case (_, v2) => throw IllegalStateException(s"Expected set but got $v2")
    }

  override def intersect(sets: Seq[Value]): Value =
    sets.foldLeft[Value](BoundedSetV.Top) {
      case (Value.Top, _) | (_, Value.Top) => Value.Top
      case (BoundedSetV.Empty, _) => BoundedSetV.Empty
      case (_, BoundedSetV.Empty) => BoundedSetV.Empty
      case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) =>
        except.tryCatch {
          BoundedSetV.NonEmpty(meetV(b1, b2).get)
        } /* catch */ { exec =>
          // Meet produced bot, that is, the set is empty
          BoundedSetV.Empty
        }(using withJoinV)
      case (_, s) => throw IllegalStateException(s"Expected set but got $s")
    }

  override def iter(s: Value)(values: Set[Value] => AbstractRelation)(empty: => AbstractRelation): AbstractRelation =
    s match
      case Value.Top =>
        effects.joinComputations {
          values(Set(Value.Top))
        } {
          empty
        }(using joinRV)
      case BoundedSetV.Empty =>
        empty
      case BoundedSetV.NonEmpty(bound) =>
        values(Set(bound))
      case _ => throw IllegalArgumentException(s"Expected set but got $s")

trait BoundedEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (BoundedSetV.Empty, BoundedSetV.Empty) => Topped.Actual(true)
    case (BoundedSetV.Empty, _: BoundedSetV.NonEmpty) => Topped.Actual(false)
    case (_: BoundedSetV.NonEmpty, BoundedSetV.Empty) => Topped.Actual(false)
    case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) => equ(b1, b2)
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (BoundedSetV.Empty, BoundedSetV.Empty) => Topped.Actual(false)
    case (BoundedSetV.Empty, _: BoundedSetV.NonEmpty) => Topped.Actual(true)
    case (_: BoundedSetV.NonEmpty, BoundedSetV.Empty) => Topped.Actual(true)
    case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) => neq(b1, b2)
    case _ => super.neq(v1, v2)

trait BoundedJoinV(using eqOps: EqOps[Value, Topped[Boolean]]) extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (BoundedSetV.Empty, _) => rhs
    case (_, BoundedSetV.Empty) => lhs
    case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) => BoundedSetV.NonEmpty(combine(b1, b2))
    case _ => super.combine(lhs, rhs)

trait BoundedMeetV[J[_] <: MayJoin[?]](using eqOps: EqOps[Value, Topped[Boolean]], except: Except[BaseIRException, ?, J], mayJoinV: J[Value]) extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (BoundedSetV.Empty, _) => BoundedSetV.Empty
    case (_, BoundedSetV.Empty) => BoundedSetV.Empty
    case (BoundedSetV.NonEmpty(b1), BoundedSetV.NonEmpty(b2)) =>
      except.tryCatch {
        BoundedSetV.NonEmpty(meet(b1, b2))
      } /* catch */ { exec =>
        // Meet produced Bot
        BoundedSetV.Empty
      }(using mayJoinV)

    case _ => super.meet(lhs, rhs)

trait BoundedAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]
  with RequireMeet[Value]
  with RequireJoin[Value]:

  lazy val setOps: SetOps[Value, AbstractRelation, Topped[Boolean]] = BoundedSetVOps(using eqOps, effects, except, joinV, meetV, mayJoinV, joinRV)
