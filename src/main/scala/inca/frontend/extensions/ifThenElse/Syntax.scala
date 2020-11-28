package inca.frontend.extensions.ifThenElse

import inca.frontend.core
import inca.frontend.parser.SourceLocation

trait Syntax extends core.Syntax {
  type ElseIf <: SourceLocation

  def IfThenElse(cond: Expression, thn: Body, elseIfs: Seq[ElseIf], els: Option[Body]): Statement
  def ElseIf(cond: Expression, body: Body): ElseIf
}
