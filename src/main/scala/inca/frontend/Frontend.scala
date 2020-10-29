package inca.frontend

import inca.frontend.core.Core.Module
import inca.frontend.desugar.Desugarable
import inca.frontend.extensions._
import inca.frontend.parser.CoreParser
import inca.frontend.typechecker.CoreTypechecker
import inca.runtime.context.LanguageMetaInfo

trait Frontend extends CoreParser with CoreTypechecker {

  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables

  protected def desugarables: Seq[Desugarable] = Seq()

  def parseModule(code: String): fastparse.Parsed[Module] = {
    fastparse.parse(code, module(_))
  }
}

abstract class BaseFrontend(val lang: LanguageMetaInfo) extends Frontend

object Frontend {
  def Core(lang: LanguageMetaInfo): Frontend =
    new BaseFrontend(lang) with Frontend

  def Inca(lang: LanguageMetaInfo): Frontend =
    new BaseFrontend(lang)
      with BoolOpsFrontend
      with DataOpCallFrontentd
      with EnumFrontend
      with ForallExistsFrontend
      with ForeachFrontend
      with IfThenElseFrontend
      with MatchFrontend
      with SwitchFrontend
}
