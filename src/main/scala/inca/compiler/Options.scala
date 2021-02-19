package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultFrontend, defaultFrontend_old, defaultOptimizations}
import inca.frontend.Frontend
import inca.frontend_old
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo,
                   frontendFactory: LanguageMetaInfo => Frontend = defaultFrontend,
                   frontendFactory_old: LanguageMetaInfo => frontend_old.core.Frontend = defaultFrontend_old,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: Frontend = frontendFactory(languageMetaInfo)
  def frontendOld: frontend_old.core.Frontend = frontendFactory_old(languageMetaInfo)
}

object Options {
  val defaultFrontend: LanguageMetaInfo => Frontend =
    Frontend.Core

  val defaultFrontend_old: LanguageMetaInfo => frontend_old.core.Frontend =
    frontend_old.core.Frontend.Core

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}