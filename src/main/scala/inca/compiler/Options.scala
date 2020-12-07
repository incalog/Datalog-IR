package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultFrontend, defaultOptimizations}
import inca.frontend.core.Frontend
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo,
                   frontendFactory: LanguageMetaInfo => Frontend = defaultFrontend,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: Frontend = frontendFactory(languageMetaInfo)
}

object Options {
  val defaultFrontend: LanguageMetaInfo => Frontend =
    Frontend.Inca

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}