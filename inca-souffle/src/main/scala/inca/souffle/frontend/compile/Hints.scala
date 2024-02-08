package inca.souffle.frontend

import inca.ir.Hint
import inca.ir.Hint.Key
import inca.souffle.syntax.QueryPlan
import inca.souffle.syntax.ProgramContent.Directive


case class SouffleQueryPlanHint(qp: QueryPlan) extends Hint, Hint.Key:
  override def key: Key = this
