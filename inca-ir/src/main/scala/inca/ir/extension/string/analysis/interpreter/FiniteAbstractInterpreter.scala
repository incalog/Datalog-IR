package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.BaseIRException
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{BaseJoinV, BaseMeetV, BaseWidenV, FiniteAbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.FiniteIntOps
import sturdy.effect.failure.Failure
import sturdy.values.Powerset
import sturdy.data.MayJoin
import sturdy.values.Topped
import sturdy.data.WithJoin
import sturdy.effect.except.Except

// This is the maximum allowed string lengths for strings in the edb
val edbStringLengthUpperBound = 50
// We need to provide an approximation for ordinal numbers. The smallest one is always 0, while the biggest one is:
val maxOrdinalNumber = Int.MaxValue - 2
// This is the maximum allowed Strings length. If a string gets larger than this, we widen.
val maxStringLength = 65536*10


trait Component
case class Str(s: String) extends Component:
  override def toString: String = s
case object Unknown extends Component:
  override def toString: String = "$?"

case class FiniteStringV(components: Seq[Component], length: Int) extends Value:
  override def isConstant: Boolean = !components.contains(Unknown)
  override def toString: String = s"\"${components.mkString("")}\" | $length"
  override def isFinite: Boolean = true

object FiniteStringV:
  def apply(length: Int): FiniteStringV = new FiniteStringV(Seq(Unknown), length)
  def edb(maxLength: Int = edbStringLengthUpperBound): FiniteStringV = new FiniteStringV(Seq(Unknown), maxLength)
  def lit(s: String): FiniteStringV = new FiniteStringV(Seq(Str(s)), s.length)

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
      val newLength = f1.length.max(f2.length)
      if (f1.components == f2.components)
        FiniteStringV(f1.components, newLength)
      else
        FiniteStringV(newLength)
    case _ => super.combine(lhs, rhs)

trait FiniteStringWidenV extends BaseWidenV:
  override def combine(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newLength = f1.length.max(f2.length)
      if (newLength > maxStringLength)
        println(s"WIDEN!!!! :: $newLength")
        Value.Top
      else if (f1.components == f2.components)
        FiniteStringV(f1.components, newLength)
      else
        FiniteStringV(newLength)
    case _ => super.combine(lhs, rhs)

trait FiniteStringMeetV extends BaseMeetV:
  override def meet(lhs: Value, rhs: Value): Value = (lhs, rhs) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
      val newLength = f1.length.min(f2.length)
      if (f1.components == f2.components)
        FiniteStringV(f1.components, newLength)
      else
        FiniteStringV(newLength)
    case _ => super.meet(lhs, rhs)

class FiniteStringVOps(using failure: Failure, except: Except[BaseIRException, ?, ?], intOps: FiniteIntOps[Int, Value]) extends StringOps[Topped[Boolean], Value]:
  override def stringLit(s: String): Value = FiniteStringV.lit(s)

  override def toString(v: Value): Value = v match
    case Value.Top => Value.Top
    case _: FiniteStringV => v
    case _ => Value.Top // TODO: Maybe we know its finite... I'm not sure

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (f1: FiniteStringV, f2: FiniteStringV) =>
        FiniteStringV(f1.components ++ f2.components, f1.length + f2.length)
    case (Value.Top, _) | (_, Value.Top) => Value.Top
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

  override def substring(v: Value, index: Value, length: Value): Value = v match
    case f: FiniteStringV =>
      val newLength = intOps.integerValue(length).getOrElse(f.length)
      FiniteStringV(newLength)
    case Value.Top => Value.Top
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def stringLength(v: Value): Value = v match
    case f: FiniteStringV if f.isConstant =>
      intOps.integerLit(f.toString.length)
    case f: FiniteStringV =>
      // Length of IDB strings is known. This is the lower bound for the length
      val lowerBound = f.components.map {
        case Str(s) => s.length
        case _ => 0
      }.sum
      intOps.interval(lowerBound, f.length)
    case Value.Top => Value.Top
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def ordinalNumber(v: Value): Value = v match
    case Value.Top => Value.Top
    case f: FiniteStringV if f.isConstant => intOps.integerLit(f.toString.hashCode.abs)
    case f: FiniteStringV => intOps.interval(0, maxOrdinalNumber)
    case _ => failure(InvalidStringValue, s"Can not get ordinal number of $v")

  override def matches(v: Value, pattern: Value): Topped[Boolean] = (v, pattern) match
    case (Value.Top, _) | (_, Value.Top) => Topped.Top
    case (f1: FiniteStringV, f2: FiniteStringV) => Topped.Top
    case _ => failure(InvalidStringConcat, s"Can not regex match values $v and $pattern")

  override def stringValue(v: Value): String = v match
    case f: FiniteStringV if f.isConstant  => f.toString
    case _ => failure(InvalidStringValue, s"Value $v has no string value")


trait FiniteStringAbstractInterpreter extends GenericInterpreter[Value, Topped[Boolean], FiniteAbstractRelation, Powerset[BaseIRException], WithJoin]:
  val intOps: FiniteIntOps[Int, Value]
  lazy val stringOps: StringOps[Topped[Boolean], Value] = FiniteStringVOps(using failure, except, intOps)
