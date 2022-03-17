package inca.compiler

import inca.backend.optimize._
import inca.backend.transform.Transformation
import org.eclipse.viatra.query.runtime.rete.matcher.{ReteBackendFactory, TimelyReteBackendFactory}

trait Options {
  def optimizations: Seq[Optimization]

  def transformations: Seq[Transformation]

  def stopOnError: Boolean
  def stopOnWarning: Boolean

  def mode: ReteBackendFactory

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
  def apply(stopOnError: Boolean = true, stopOnWarning: Boolean = false, mode: ReteBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL, opts: Seq[Optimization] = Seq(), trans: Seq[Transformation] = Seq()): Options = {
    val _stopOnError = stopOnError
    val _stopOnWarning = stopOnWarning
    val _mode = mode
    new Options {
      override def optimizations: Seq[Optimization] = opts
      override def transformations: Seq[Transformation] = trans
      override def stopOnError: Boolean =  _stopOnError
      override def stopOnWarning: Boolean = _stopOnWarning
      override def mode: ReteBackendFactory = _mode
      override def withOptimizations(opts: Seq[Optimization]): Options =
        Options(stopOnError, stopOnWarning, mode, opts, trans)
      override def withTransformations(trans: Seq[Transformation]): Options =
        Options(stopOnError, stopOnWarning, mode, opts, trans)
    }
  }

}