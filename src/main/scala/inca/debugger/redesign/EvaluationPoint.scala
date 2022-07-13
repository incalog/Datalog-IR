package inca.debugger.redesign

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable
import inca.debugger.Value

trait EvaluationPoint {
  def pred: String
}

case class BeforeRule(
    pred: String,
    argBindings: ImmutableTable[Value],
    predResult: ImmutableTable[Value],
    rules: Seq[Datalog.Body])
    extends EvaluationPoint

case class InRule(
    pred: String,
    argBindings: ImmutableTable[Value],
    predResult: ImmutableTable[Value],
    current: RuleEvaluation,
    remainingRules: Seq[Datalog.Body])
    extends EvaluationPoint

case class RuleEvaluation(ruleResult: ImmutableTable[Value], atoms: Seq[Datalog.Atom])

case class EvaluationResult(pred: String, predResult: ImmutableTable[Value]) extends EvaluationPoint
