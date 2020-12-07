package inca.frontend.extensions.switch_

import inca.frontend.core.tree._
import inca.frontend.extensions.switch_.Trees.Switch
import inca.frontend.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend] def keywords: Set[String] = super.keywords + "switch"

  override protected[frontend] def statement[_: P]: P[Statement] =
    P("switch" ~ body.rep(sep = "union")).mapWithLoc(Switch) | super.statement
}
