package inca.frontend.constraint.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.constraint.compiler.ConstraintOptions.defaultDesugarables
import inca.frontend.constraint.desugar.Desugarable
import inca.frontend.constraint.extensions
import org.eclipse.viatra.query.runtime.rete.matcher.ReteBackendFactory
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

case class ConstraintOptions(
    optimizations: Seq[Optimization] = defaultOptimizations,
    transformations: Seq[Transformation] = Seq(),
    desugarables: Seq[Desugarable] = defaultDesugarables,
    stopOnError: Boolean = true,
    stopOnWarning: Boolean = false,
    mode: ReteBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    extends Options {

  override def withOptimizations(opts: Seq[Optimization]): ConstraintOptions =
    ConstraintOptions(
      opts,
      transformations,
      desugarables,
      stopOnError,
      stopOnWarning,
      mode
    )

  override def withTransformations(trans: Seq[Transformation]): ConstraintOptions =
    ConstraintOptions(
      optimizations,
      trans,
      desugarables,
      stopOnError,
      stopOnWarning,
      mode
    )

  override def withEngine(_mode: ReteBackendFactory): Options =
    ConstraintOptions(
      optimizations,
      transformations,
      desugarables,
      stopOnError,
      stopOnWarning,
      _mode
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
