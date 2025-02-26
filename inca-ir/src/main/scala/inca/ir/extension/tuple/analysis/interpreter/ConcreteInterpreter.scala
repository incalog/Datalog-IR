package inca.ir.extension.tuple.analysis.interpreter

import inca.ir.analysis.base.effect.{BaseIRException, BaseIRFailure}
import inca.ir.analysis.base.values.{ConcreteRelation, Value}
import inca.ir.extension.data.{CaseDefinitionReference, DataDefinitionReference}
import inca.ir.extension.string.analysis.interpreter.InvalidStringConcat
import sturdy.values.Powerset
import sturdy.data.MayJoin.NoJoin
import sturdy.effect.failure.Failure

case class CTupleV(ts: Seq[Value]) extends Value:
  override def toString: String = ts.mkString("(", ",", ")")
  override def isConstant: Boolean = ts.forall(_.isConstant)

private class CTupleVOps(using failure: Failure) extends TupleOps[Value]:
  override def tupleLit(ts: Seq[Value]): Value = CTupleV(ts)
  override def project(t: Value, index: Int): Value = t match
    case CTupleV(ts) if index >= 0 && (index < ts.size) => ts(index)
    case CTupleV(ts) => failure(InvalidTupleProjection, s"Index $index out of bounds")
    case _ => failure(InvalidTupleProjection, s"Expected a tuple, but got $t")

trait ConcreteInterpreter extends GenericInterpreter[Value, Boolean, ConcreteRelation[Value], Powerset[BaseIRException], NoJoin]:
  val dataOps: TupleOps[Value] = new CTupleVOps()
