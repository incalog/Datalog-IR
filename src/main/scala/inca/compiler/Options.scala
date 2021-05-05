package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultDesugarables, defaultOptimizations}
import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions
import inca.runtime.context.DataModel
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options.defaultOptimizations
import inca.frontend.functional.Frontend
import inca.runtime.context.DataModel

case class Options(optimizations: Seq[Optimization] = defaultOptimizations,
                   transformations: Seq[Transformation] = Seq(),
                   desugarables: Seq[Desugarable] = defaultDesugarables,
                   stopOnError: Boolean = true,
                   stopOnWarning: Boolean = false) {
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

  val defaultDesugarables: Seq[Desugarable] = Seq(
    extensions.boolOps.Desugaring,
    extensions.evalCall.Desugaring,
    extensions.forallExists.Desugaring,
    extensions.foreach.Desugaring,
    extensions.ifThenElse.Desugaring,
    extensions.match_.Desugaring,
    extensions.switch_.Desugaring
  )
}