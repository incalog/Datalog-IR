package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.{AtomFailed, BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ARelationValue, BaseJoinV, CRelationValue, Top, VBool, Value}
import sturdy.effect.{Effect, EffectStack}
import sturdy.effect.failure.Failure
import sturdy.values.{Powerset, Topped}
import sturdy.data.MayJoin.{NoJoin, WithJoin}
import sturdy.effect.except.Except
import sturdy.values.ordering.EqOps

case object InvalidDeconstruct extends BaseIRFailure

case class CDataV(dataName: String, caseName: String, args: Seq[Value]) extends Value:
  override def toString: String = s"$caseName${args.mkString("(", ", ", ")")}"

private class CDataVOps[R] extends DataOps[Value, R]:
  override def construct(dataName: String, caseName: String, args: Seq[Value]): Value = CDataV(dataName, caseName, args)
  override def deconstruct(v: Value, dataName: String, caseName: String)(matching: Seq[Value] => R)(notMatching: => R): R = v match
    case CDataV(`dataName`, `caseName`, cArgs) => matching(cArgs)
    case _ => notMatching


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, CRelationValue[Value], Powerset[BaseIRException], NoJoin]:
  val dataOps: DataOps[Value, CRelationValue[Value]] = new CDataVOps
