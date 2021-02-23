package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation, RemoveBodyOfUnusedDataConstructor}
import inca.backend.transform.{Transformation, UndefElimination}
import inca.compiler.Options.{defaultFrontend, defaultFrontend_old, defaultOptimizations}
import inca.frontend.Frontend
import inca.frontend_old
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo = null,
                   frontendFactory: () => Frontend = defaultFrontend,
                   frontendFactory_old: LanguageMetaInfo => frontend_old.core.Frontend = defaultFrontend_old,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   transformations: Seq[Transformation] = Seq(),
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: Frontend = frontendFactory()
  def frontendOld: frontend_old.core.Frontend = frontendFactory_old(languageMetaInfo)
}

object Options {
  val defaultFrontend: () => Frontend =
    () => Frontend.Core

  val defaultFrontend_old: LanguageMetaInfo => frontend_old.core.Frontend =
    frontend_old.core.Frontend.Inca

  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints
  )

  val defaultTransformations: Seq[Transformation] = Seq(
    RemoveBodyOfUnusedDataConstructor,
    AdornProgram,
    MagicSetTransformation,
    UndefElimination
  )
}