package inca.frontend.extensions.ifThenElse

import inca.frontend.core.tree._
import inca.frontend.extensions.ifThenElse.Trees._
import inca.frontend.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

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
