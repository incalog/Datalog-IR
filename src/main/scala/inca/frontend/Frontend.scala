package inca.frontend

import inca.frontend.core.Module
import inca.frontend.parser.Parser
import inca.frontend.typechecker.Typechecker

trait Frontend extends Parser with Typechecker {

  def parseModule(code: String): fastparse.Parsed[Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}

object Frontend {
  def Core: Frontend = new Frontend { }
}