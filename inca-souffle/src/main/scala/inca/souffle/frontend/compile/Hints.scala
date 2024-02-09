package inca.souffle.frontend

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.souffle.syntax.{DirectiveValue, QueryPlan}
import inca.souffle.syntax.ProgramContent.Directive


object SouffleQueryPlanHint extends Hint.Key
case class SouffleQueryPlanHint(qp: QueryPlan) extends Hint:
  override def key: Key = SouffleQueryPlanHint

object SouffleInputHint extends Hint.Key
case class SouffleInputHint(attrs: Map[String, DirectiveValue]) extends Hint:
  override def key: Key = SouffleInputHint
  