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
  def apply(): Options = new Options {
    override def optimizations: Seq[Optimization] = defaultOptimizations
    override def transformations: Seq[Transformation] = Seq()
    override def stopOnError: Boolean = true
    override def stopOnWarning: Boolean = false
    override def withOptimizations(opts: Seq[Optimization]): Options = this
    override def withTransformations(trans: Seq[Transformation]): Options = this
  }

  val defaultOptimizations: Seq[Optimization] = Seq(
    EliminateNonproductiveRelations,
    InlineSimpleRelations,
    ConstantPropagation,
    EliminateAliases,
    EvalFusion,
    InferVarTypes,
    FoldConstantAtoms,
    EliminateNonproductiveRelations
  )
}