package inca.debugger.redesign

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable
import inca.debugger.Value

trait EvaluationPoint {
  def pred: String
}
object EvaluationPoint {
  def toTableless(ep: EvaluationPoint): EvaluationPoint = ep match {
    case PredicateEntry(pred, _, _) =>
      PredicateEntry(pred, ImmutableTable.empty(Seq()), ImmutableTable.empty(Seq()))
    case BeforeRule(pred, _, _, rules) =>
      BeforeRule(pred, ImmutableTable.empty(Seq()), ImmutableTable.empty(Seq()), rules)
    case EvaluationResult(pred, _) =>
      EvaluationResult(pred, ImmutableTable.empty(Seq()))
    case InRule(pred, _, _, RuleEvaluation(_, ruleIdx, atoms), remRules) =>
      InRule(
        pred,
        ImmutableTable.empty(Seq()),
        ImmutableTable.empty(Seq()),
        RuleEvaluation(ImmutableTable.empty(Seq()), ruleIdx, atoms),
        remRules)
  }
}

case class PredicateEntry(
    pred: String,
    argBindings: ImmutableTable[Value],
    predResult: ImmutableTable[Value])
    extends EvaluationPoint

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

case class RuleEvaluation(ruleResult: ImmutableTable[Value], ruleIdx: Int, atoms: Seq[Datalog.Atom])

case class EvaluationResult(pred: String, predResult: ImmutableTable[Value]) extends EvaluationPoint
