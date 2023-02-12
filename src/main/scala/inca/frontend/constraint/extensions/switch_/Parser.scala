package inca.frontend.constraint.extensions.switch_

import inca.frontend.constraint.core._
import inca.frontend.constraint.extensions.switch_.Trees.Switch
import inca.frontend.constraint.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def keywords: Set[String] = super.keywords + "switch"

  override protected[frontend] def statement[_: P]: P[Statement] =
    P("switch" ~ body.rep(sep = "union")).mapWithLoc(Switch) | super.statement
}
