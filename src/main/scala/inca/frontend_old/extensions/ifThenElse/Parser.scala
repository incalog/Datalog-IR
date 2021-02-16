package inca.frontend_old.extensions.ifThenElse

import inca.frontend_old.core.tree._
import inca.frontend_old.extensions.ifThenElse.Trees._
import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend_old] def keywords: Set[String] = super.keywords ++ Seq("if", "else")

  override protected[frontend_old] def statement[_: P]: P[Statement] = {
    ifThenElse |
      super.statement
  }

  protected[frontend_old] def ifThenElse[_: P]: P[Statement] =
    P(
      "if" ~ "(" ~ exp ~ ")" ~ body ~
        elseif.rep.? ~ P("else" ~ body).?
    ).mapWithLoc { case (e, b, eifs, el) => IfThenElse(e, b, eifs.getOrElse(Seq.empty), el) }


  protected[frontend_old] def elseif[_: P]: P[ElseIf] =
    P(
      "else" ~ "if" ~ "(" ~ exp ~ ")" ~ body
    ).mapWithLoc { case (e, b) => ElseIf(e, b) }
}
