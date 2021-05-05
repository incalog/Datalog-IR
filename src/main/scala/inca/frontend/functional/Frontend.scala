package inca.frontend.functional

import inca.frontend.functional.core.Module
import inca.frontend.functional.parser.Parser
import inca.frontend.functional.typechecker.Typechecker

trait Frontend extends Parser with Typechecker {

  def parseModule(code: String): fastparse.Parsed[Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}

object Frontend {
  def Core: Frontend = new Frontend { }
}