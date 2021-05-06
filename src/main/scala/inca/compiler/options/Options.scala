package inca.compiler.options

import inca.backend.optimize._
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions

trait Options {
  def optimizations: Seq[Optimization]

  def transformations: Seq[Transformation]

  def stopOnError: Boolean

  def stopOnWarning: Boolean
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