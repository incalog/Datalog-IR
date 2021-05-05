package inca.compiler

import inca.backend.optimize._
import inca.compiler.Options.{defaultDesugarables, defaultOptimizations, defaultTransformations}
import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions
import inca.runtime.context.DataModel
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.frontend.functional.Frontend
import inca.runtime.context.DataModel

trait Options {
  def optimizations: Seq[Optimization]
  def transformations: Seq[Transformation]
  def stopOnError: Boolean
  def stopOnWarning: Boolean
}

case class ConstraintOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = Seq(),
                             desugarables: Seq[Desugarable] = defaultDesugarables,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options

case class FunctionalOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = defaultTransformations,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options


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