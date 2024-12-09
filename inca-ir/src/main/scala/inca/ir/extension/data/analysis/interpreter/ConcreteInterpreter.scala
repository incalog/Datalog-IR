package inca.ir.extension.data.analysis.interpreter

import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference}
import sturdy.values.Powerset
import sturdy.data.MayJoin.NoJoin

case object InvalidDeconstruct extends BaseIRFailure

case class CDataV(dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference, args: Seq[Value]) extends Value:
  override def toString: String = s"${caseDef.name}${args.mkString("(", ",", ")")}"

private class CDataVOps[R] extends DataOps[Value, R]:
  override def construct(dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference, args: Seq[Value]): Value = CDataV(dataDef, caseDef, args)

  override def deconstruct(v: Value, dataDef: DataDefinitionReference, caseDef: CaseDefinitionReference)(matching: Seq[Value] => R)(notMatching: => R): R = v match
    case CDataV(`dataDef`, `caseDef`, cArgs) => matching(cArgs)
    case _ => notMatching


trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], Powerset[BaseIRException], NoJoin]:
  val dataOps: DataOps[Value, ConcreteRelation[Value]] = new CDataVOps
