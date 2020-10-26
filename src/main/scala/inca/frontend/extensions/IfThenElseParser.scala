package inca.frontend.extensions

import fastparse.ScalaWhitespace._
import fastparse._
import inca.frontend.core.Core.Statement
import inca.frontend.parser.CoreParser

/**
 * Extension adding "ifthenelse" statements to @see Parser.
 */
trait IfThenElseParser extends CoreParser {


  /**
   * Statement parser
   */
  override def statement[_: P]: P[Statement] = {
    ifThenElse |
      super.statement
  }

  def ifThenElse[_: P]: P[Statement] =
    P(
      "if" ~ "(" ~ exp ~ ")" ~ body ~
        elseif.rep.? ~ P("else" ~ body).?
    ).map { case (e, b, eifs, el) => IfThenElse(e, b, eifs.getOrElse(Seq.empty), el) }

  def elseif[_: P]: P[ElseIf] =
    P(
      "else" ~ "if" ~ "(" ~ exp ~ ")" ~ body
    ).map { case (e, b) => ElseIf(e, b) }


  override def keywords: Set[String] = super.keywords ++ Seq("if", "else")

}
