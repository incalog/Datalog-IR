package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, BaseJoinV, BaseMeetV, BaseWidenV, Value}
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
  override def toString: String = components.mkString("")

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
  var maxConcatDepth: Int = 1

  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newDepth = f1.concatDepth.max(f2.concatDepth)
      if (newDepth > maxConcatDepth)
        println("Concat!!!")
        Value.Top
      else if (f1.components == f2.components)
        FiniteStringV(f1.components, newDepth)
      else
        FiniteStringV(newDepth)
    case _ => super.combine(lhs, rhs)

trait FiniteStringMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newDepth = f1.concatDepth.min(f2.concatDepth)
      if (f1.components == f2.components)
        FiniteStringV(f1.components, newDepth)
      else
        FiniteStringV(newDepth)
    case _ => super.meet(lhs, rhs)

class FiniteStringVOps(using failure: Failure, except: Except[BaseIRException, ?, ?]) extends StringOps[Value]:
  override def stringLit(s: String): Value = FiniteStringV.lit(s)

  override def toString(v: Value): Value = v match
    case Value.Top => Value.Top
    case _: FiniteStringV => v
    case _ => Value.Top // TODO: Maybe we know its finite... I'm not sure

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      if (f1.components.lastOption.contains(Unknown) && f2.components.headOption.contains(Unknown))
        // Collapse unknowns
        FiniteStringV(f1.components ++ f2.components.tail, f1.concatDepth + f2.concatDepth)
      else
        FiniteStringV(f1.components ++ f2.components, f1.concatDepth + f2.concatDepth)
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

  override def stringValue(v: Value): String = v match
    case f: FiniteStringV if f.isConstant  => f.toString
    case _ => failure(InvalidStringValue, s"Value $v has no string value")


trait FiniteStringAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], AbstractRelation, Powerset[BaseIRException], WithJoin]:
  val stringOps: StringOps[Value] = FiniteStringVOps(using failure, except)
