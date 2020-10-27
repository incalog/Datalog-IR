package inca

import inca.CompilerOptions._
import inca.backend.optimize._
import inca.frontend.Frontend
import inca.runtime.context.LanguageMetaInfo

case class CompilerOptions(languageMetaInfo: LanguageMetaInfo,
                           frontend: Frontend = defaultFrontend,
                           optimizations: Seq[Optimization] = defaultOptimizations)

object CompilerOptions {
  val defaultFrontend: Frontend = Frontend.Inca

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}