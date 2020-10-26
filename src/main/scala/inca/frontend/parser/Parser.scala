package inca.frontend.parser

import inca.frontend.core.Core.Module
import inca.frontend.extensions._

object Parser {

  val core = new CoreParser

  val full =
    new CoreParser
      with BoolOpsParser
      with CastParser
      with EnumParser
      with ForallExistsParser
      with ForeachParser
      with IfThenElseParser
      with MatchParser
      with SwitchParser

  def parseModule(code : String) : fastparse.Parsed[Module] = {
    fastparse.parse(code, full.module(_))
  }
}