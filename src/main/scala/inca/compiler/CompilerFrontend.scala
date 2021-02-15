package inca.compiler

import inca.frontend_old.core.Frontend
import inca.frontend_old.{core, extensions}
import inca.runtime.context.LanguageMetaInfo

trait CompilerFrontend extends Frontend {
  override val syntax: core.Trees
}

object CompilerFrontend {
  def Core(langInfo: LanguageMetaInfo): CompilerFrontend =
    new CompilerFrontend {
      override val lang: LanguageMetaInfo = langInfo
      override val syntax = coreTrees
    }
  val coreTrees = new core.Trees {}

  def Inca(langInfo: LanguageMetaInfo): CompilerFrontend =
    new CompilerFrontend
      with extensions.boolOps.Frontend
      with extensions.evalCall.Frontend
      //      with EnumFrontend
      with extensions.forallExists.Frontend
      with extensions.foreach.Frontend
      with extensions.ifThenElse.Frontend
      with extensions.match_.Frontend
      with extensions.switch_.Frontend {

      override val lang: LanguageMetaInfo = langInfo
      override val syntax = incaTrees
    }
  val incaTrees = new core.Trees
    with extensions.boolOps.Trees
    with extensions.evalCall.Trees
    with extensions.forallExists.Trees
    with extensions.foreach.Trees
    with extensions.ifThenElse.Trees
    with extensions.match_.Trees
    with extensions.switch_.Trees {}
}