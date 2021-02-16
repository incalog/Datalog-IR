package inca.frontend_old.extensions.switch_

import inca.frontend_old.core.tree._
import inca.frontend_old.extensions.switch_.Trees.Switch
import inca.frontend_old.parser.CoreParser

trait Parser extends CoreParser {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected[frontend_old] def keywords: Set[String] = super.keywords + "switch"

  override protected[frontend_old] def statement[_: P]: P[Statement] =
    P("switch" ~ body.rep(sep = "union")).mapWithLoc(Switch) | super.statement
}
