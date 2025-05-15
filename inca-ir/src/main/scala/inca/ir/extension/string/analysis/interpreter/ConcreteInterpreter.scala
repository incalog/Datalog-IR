package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{BaseJoinV, ConcreteRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.CIntV
import sturdy.values.integer.IntegerOps
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin.NoJoin
import sturdy.values.integer.IntegerOps

case object InvalidStringConcat extends BaseIRFailure

case object InvalidStringValue extends BaseIRFailure

case class CStringV(value: String) extends Value:
  override def toString: String = s"\"$value\""
  override def isConstant: Boolean = true

private class CStringVOps (using failure: Failure, intOps: IntegerOps[Int, Value]) extends StringOps[Value]:
  override def stringLit(s: String): Value = CStringV(s)

  override def toString(v: Value): Value = v match
    case CStringV(_) => v
    case _ => CStringV(v.toString)

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (CStringV(s1), CStringV(s2)) => CStringV(s1 + s2)
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

  override def substring(v: Value, index: Value, length: Value): Value = (v, index, length) match
    case (CStringV(s), CIntV(idx), CIntV(len)) => CStringV(s.substring(idx, idx+len))
    case _ => failure(InvalidStringValue, s"Can not get substring of $v")

  override def stringLength(v: Value): Value = v match
    case CStringV(s) => intOps.integerLit(s.length)
    case _ => failure(InvalidStringValue, s"Can not get length of $v")

  override def stringValue(v: Value): String = v match
    case CStringV(s) => s 

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], BaseIRException, NoJoin]:
  val intOps: IntegerOps[Int, Value]
  lazy val stringOps: StringOps[Value] = CStringVOps(using failure, intOps)

