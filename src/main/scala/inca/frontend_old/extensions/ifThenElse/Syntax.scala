package inca.frontend_old.extensions.ifThenElse

import inca.frontend_old.core
import inca.frontend_old.parser.SourceLocation

trait Syntax extends core.Syntax {
  type ElseIf <: SourceLocation

  def IfThenElse(cond: Expression, thn: Body, elseIfs: Seq[ElseIf], els: Option[Body]): Statement
  def ElseIf(cond: Expression, body: Body): ElseIf
}
