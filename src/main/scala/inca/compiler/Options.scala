package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.Transformation
import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.Options.{defaultFrontend, defaultOptimizations}
import inca.frontend.Frontend
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo = null,
                   frontendFactory: () => Frontend = defaultFrontend,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   transformations: Seq[Transformation] = Seq(),
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: Frontend = frontendFactory()
}

object Options {
  val defaultFrontend: () => Frontend =
    () => Frontend.Core

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )

  val defaultTransformations: Seq[Transformation] = Seq(
//    RemoveBodyOfUnusedDataConstructor,
    AdornProgram,
    MagicSetTransformation
  )
}