package inca.frontend.constraint.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.constraint.compiler.ConstraintOptions.defaultDesugarables
import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions

case class ConstraintOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = Seq(),
                             desugarables: Seq[Desugarable] = defaultDesugarables,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options {
  override def withOptimizations(opts: Seq[Optimization]): Options =
    ConstraintOptions(
      opts,
      transformations,
      desugarables,
      stopOnError,
      stopOnWarning
    )

  override def withTransformations(trans: Seq[Transformation]): Options =
    ConstraintOptions(
      optimizations,
      trans,
      desugarables,
      stopOnError,
      stopOnWarning
    )
}

object ConstraintOptions {
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
