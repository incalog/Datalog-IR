package inca.ir.extension.string.analysis.interpreter

import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{BaseJoinV, ConcreteRelation, Top, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin.NoJoin

case object InvalidStringConcat extends BaseIRFailure

case class CStringV(value: String) extends Value:
  override def toString: String = value

private class CStringVOps (using failure: Failure) extends StringOps[Value]:
  override def stringLit(s: String): Value = CStringV(s)

  override def toString(v: Value): Value = CStringV(v.toString)

  override def concat(v1: Value, v2: Value): Value = (v1, v2) match
    case (CStringV(s1), CStringV(s2)) => CStringV(s1 + s2)
    case _ => failure(InvalidStringConcat, s"Can not concat non-string values $v1 and $v2")

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], Powerset[BaseIRException], NoJoin]:
  val stringOps: StringOps[Value] = CStringVOps(using failure)
