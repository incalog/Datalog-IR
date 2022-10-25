package inca.frontend.souffle.debugger

import inca.compiler.source.{SourceLocation, SourceObject}
import inca.debugger.redesign.EvaluationPoint
import inca.frontend.souffle.Syntax.{Input, RuleDefinition, RuleSignature}

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
