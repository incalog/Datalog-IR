package inca.frontend.constraint.extensions.ifThenElse

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.ifThenElse.Trees._
import inca.frontend.constraint.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse._
  import fastparse.ScalaWhitespace._

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("if", "else")

  override protected[frontend] def statement[_: P]: P[Statement] = {
    ifThenElse |
      super.statement
  }

  protected[frontend] def ifThenElse[_: P]: P[Statement] =
    P(
      "if" ~ "(" ~ exp ~ ")" ~ body ~
        elseif.rep.? ~ P("else" ~ body).?
    ).mapWithLoc { case (e, b, eifs, el) => IfThenElse(e, b, eifs.getOrElse(Seq.empty), el) }

  protected[frontend] def elseif[_: P]: P[ElseIf] =
    P(
      "else" ~ "if" ~ "(" ~ exp ~ ")" ~ body
    ).mapWithLoc { case (e, b) => ElseIf(e, b) }
}
