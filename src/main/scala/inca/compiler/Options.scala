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
    EvalFusion,
    InferVarTypes,
    FoldConstantAtoms,
    EliminateNonproductiveRelations
  )
  def apply(_stopOnError: Boolean, _stopOnWarning: Boolean, _opts: Seq[Optimization] = Seq(), _trans: Seq[Transformation] = Seq()): Options = new Options {
    override def optimizations: Seq[Optimization] = _opts
    override def transformations: Seq[Transformation] = _trans
    override def stopOnError: Boolean =  _stopOnError
    override def stopOnWarning: Boolean = _stopOnWarning
    override def withOptimizations(opts: Seq[Optimization]): Options =
      Options(_stopOnError, _stopOnWarning, opts, _trans)
    override def withTransformations(trans: Seq[Transformation]): Options =
      Options(_stopOnError, _stopOnWarning, _opts, trans)
  }

}