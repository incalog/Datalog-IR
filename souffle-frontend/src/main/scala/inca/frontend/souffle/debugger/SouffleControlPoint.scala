package inca.frontend.souffle.debugger

import inca.compiler.source.SourceLocation
import inca.compiler.source.SourceObject
import inca.debugger.redesign_old.EvaluationPoint
import inca.frontend.souffle.Syntax.Input
import inca.frontend.souffle.Syntax.RuleDefinition
import inca.frontend.souffle.Syntax.RuleSignature

sealed trait SouffleControlPoint {
  val rel: RuleSignature
  val point: SourceObject
  def region: SourceLocation
}
case class PatternEndPoint(rel: RuleSignature, point: SourceObject, irPoint: EvaluationPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = point.loc
}
case class InputPoint(rel: RuleSignature, in: Input, point: SourceObject, irPoint: EvaluationPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = in
}
case class InRulePoint(
    rel: RuleSignature,
    rule: RuleDefinition,
    point: SourceObject,
    irPoint: EvaluationPoint)
    extends SouffleControlPoint {
  override def region: SourceLocation = rule
}
