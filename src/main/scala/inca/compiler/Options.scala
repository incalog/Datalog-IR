package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultFrontend, defaultOptimizations}
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo,
                   frontendFactory: LanguageMetaInfo => CompilerFrontend = defaultFrontend,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: CompilerFrontend = frontendFactory(languageMetaInfo)
}

object Options {
  val defaultFrontend: LanguageMetaInfo => CompilerFrontend =
    CompilerFrontend.Inca

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )
}