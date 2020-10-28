package inca

import inca.CompilerOptions._
import inca.backend.optimize._
import inca.frontend.Frontend
import inca.runtime.context.LanguageMetaInfo

case class CompilerOptions(languageMetaInfo: LanguageMetaInfo,
                           frontendFactory: LanguageMetaInfo => Frontend = defaultFrontend,
                           optimizations: Seq[Optimization] = defaultOptimizations) {
  def frontend: Frontend = frontendFactory(languageMetaInfo)
}

object CompilerOptions {
  val defaultFrontend: LanguageMetaInfo => Frontend =
    Frontend.Inca

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}