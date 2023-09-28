package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.Transformation

trait Options {
  def optimizations: Seq[Optimization]

  def transformations: Seq[Transformation]

  def stopOnError: Boolean

  def stopOnWarning: Boolean

  def withOptimizations(opts: Seq[Optimization]): Options
  def withTransformations(trans: Seq[Transformation]): Options
}

object Options {

  val defaultOptimizations: Seq[Optimization] = Seq(
    EliminateNonproductiveRelations,
    InlineSimpleRelations,
    ConstantPropagation,
    EliminateAliases,
    //EvalFusion,
    InferVarTypes,
    FoldConstantAtoms,
    EliminateNonproductiveRelations,
    ExtractLargeBodies
  )
}