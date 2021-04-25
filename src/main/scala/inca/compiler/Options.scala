package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options.defaultOptimizations
import inca.frontend.Frontend
import inca.runtime.context.LanguageMetaInfo

case class Options(languageMetaInfo: LanguageMetaInfo = null,
                   optimizations: Seq[Optimization] = defaultOptimizations,
                   transformations: Seq[Transformation] = Seq(),
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
  def frontend: Frontend = new Frontend { }
}

object Options {
  val defaultOptimizations: Seq[Optimization] = Seq(
    ConstantPropagation,
    EliminateAliases,
    InferVarTypes,
    FoldConstantConstraints,
    EliminateEmptyRelations
  )

  val defaultTransformations: Seq[Transformation] = Seq(
//    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}