package inca.frontend.core

import inca.frontend.desugar.Desugarable
import inca.frontend.extensions
import inca.frontend.parser.CoreParser
import inca.frontend.typechecker.CoreTypechecker
import inca.runtime.context.LanguageMetaInfo

trait Frontend extends CoreParser with CoreTypechecker {
  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables

  protected def desugarables: Seq[Desugarable] = Seq()

  def parseModule(code: String): fastparse.Parsed[tree.Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}

object Frontend {
  def Core(langInfo: LanguageMetaInfo): Frontend =
    new Frontend {
      override val lang: LanguageMetaInfo = langInfo
    }

  def Inca(langInfo: LanguageMetaInfo): Frontend =
    new Frontend
      with extensions.boolOps.Frontend
      with extensions.evalCall.Frontend
      //      with EnumFrontend
      with extensions.forallExists.Frontend
      with extensions.foreach.Frontend
      with extensions.ifThenElse.Frontend
      with extensions.match_.Frontend
      with extensions.switch_.Frontend {

      override val lang: LanguageMetaInfo = langInfo
    }
}