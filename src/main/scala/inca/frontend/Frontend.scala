package inca.frontend

import inca.frontend.core.Module
import inca.frontend.parser.Parser
import inca.frontend.typechecker.Typechecker
import inca.runtime.context.LanguageMetaInfo

trait Frontend extends Parser with Typechecker {

  def parseModule(code: String): fastparse.Parsed[Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}

object Frontend {
  def Core(langInfo: LanguageMetaInfo): Frontend =
    new Frontend {
      override val lang: LanguageMetaInfo = langInfo
    }

}