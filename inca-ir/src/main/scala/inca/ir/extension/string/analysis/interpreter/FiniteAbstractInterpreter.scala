package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, BaseWidenV, FiniteAbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.{IntOps, finiteUpperBound}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.values.floating.{FloatOps, LiftedFloatOps, ToppedFloatOps, given}
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.values.booleans.BooleanOps
import sturdy.values.integer.{ConcreteIntegerOps, IntegerOps, LiftedIntegerOps, ToppedIntegerOps}
import sturdy.values.ordering.{LiftedOrderingOps, OrderingOps, ToppedCertainOrderingOps}
import sturdy.data.{MakeJoined, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.Topped.Top
import sturdy.values.integer.given_OrderingOps_Int_Boolean

trait Component
case class Str(s: String) extends Component:
  override def toString: String = s
case object Unknown extends Component:
  override def toString: String = "$?"

case class FiniteStringV(components: Seq[Component], concatDepth: Int) extends Value:
  override def isConstant: Boolean = !components.contains(Unknown)
  override def toString: String = s"\"${components.mkString("")}\" | $concatDepth"
  override def isFinite: Boolean = true

object FiniteStringV:
  def apply(concatDepth: Int): FiniteStringV = new FiniteStringV(Seq(Unknown), concatDepth)
  def edb(): FiniteStringV = new FiniteStringV(Seq(Unknown), 0)
  def lit(s: String): FiniteStringV = new FiniteStringV(Seq(Str(s)), 0)

trait FiniteStringEqOps extends BaseEqOps:
  override def equ(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      if (f1.isConstant && f2.isConstant)
        Topped.Actual(f1.toString == f2.toString)
      else
        Topped.Top
    case _ => super.equ(v1, v2)

  override def neq(v1: Value, v2: Value): Topped[Boolean] = (v1, v2) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      if (f1.isConstant && f2.isConstant)
        Topped.Actual(f1.toString != f2.toString)
      else
        Topped.Top
    case _ => super.neq(v1, v2)

trait FiniteStringJoinV extends BaseJoinV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newDepth = f1.concatDepth.max(f2.concatDepth)
      if (f1.components == f2.components)
        FiniteStringV(f1.components, newDepth)
      else
        FiniteStringV(newDepth)
    case _ => super.combine(lhs, rhs)

trait FiniteStringWidenV extends BaseWidenV:
  var maxConcatDepth: Int = 20

  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newDepth = f1.concatDepth.max(f2.concatDepth)
      if (newDepth > maxConcatDepth)
        Value.Top
      else if (f1.components == f2.components)
        FiniteStringV(f1.components, newDepth)
      else
        FiniteStringV(newDepth)
    case _ => super.combine(lhs, rhs)

trait FiniteStringMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      if (f1.isConstant)
        f1
      else if (f2.isConstant)
        f2
      else
        val newDepth = f1.concatDepth.min(f2.concatDepth)
        if (f1.components == f2.components)
          FiniteStringV(f1.components, newDepth)
        else
          FiniteStringV(newDepth)
    case _ => super.meet(lhs, rhs)

class FiniteStringVOps(using failure: Failure, except: Except[BaseIRException, ?, ?], intOps: IntOps[Int, Value]) extends StringOps[Topped[Boolean], Value]:
  override def stringLit(s: String): Value = FiniteStringV.lit(s)

  override def toString(v: Value): Value = v match
    case Value.Top => Value.Top
    case _: FiniteStringV => v
    case _ => Value.Top // TODO: Maybe we know its finite... I'm not sure

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      if (f1.components.lastOption.contains(Unknown) && f2.components.headOption.contains(Unknown))
        // Collapse unknowns
        FiniteStringV(f1.components ++ f2.components.tail, f1.concatDepth + f2.concatDepth + 1)
      else
        FiniteStringV(f1.components ++ f2.components, f1.concatDepth + f2.concatDepth + 1)
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

  override def substring(v: Value, index: Value, length: Value): Value = v match
    case f: FiniteStringV if f.isConstant => FiniteStringV(f.concatDepth)
    case f: FiniteStringV => f
    case Value.Top => Value.Top
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def stringLength(v: Value): Value = v match
    case f: FiniteStringV if f.isConstant =>  intOps.integerLit(f.toString.length)
    case f: FiniteStringV => intOps.integerLit(finiteUpperBound)
    case Value.Top => Value.Top
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def ordinalNumber(v: Value): Value = v match
    case Value.Top => Value.Top
    case f: FiniteStringV if f.isConstant => intOps.integerLit(f.toString.hashCode)
    case f: FiniteStringV => intOps.integerLit(finiteUpperBound)
    case _ => failure(InvalidStringValue, s"Can not get ordinal number of $v")

  override def matches(v: Value, pattern: Value): Topped[Boolean] = (v, pattern) match
    case (Value.Top, _) | (_, Value.Top) => Topped.Top
    case (f1: FiniteStringV, f2: FiniteStringV) => Topped.Top
    case _ => failure(InvalidStringConcat, s"Can not regex match values $v and $pattern")

  override def stringValue(v: Value): String = v match
    case f: FiniteStringV if f.isConstant  => f.toString
    case _ => failure(InvalidStringValue, s"Value $v has no string value")


trait FiniteStringAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: IntOps[Int, Value]
  lazy val stringOps: StringOps[Topped[Boolean], Value] = FiniteStringVOps(using failure, except, intOps)
