package inca.frontend.functional.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.functional.compiler.FunctionalOptions.defaultTransformations
import org.eclipse.viatra.query.runtime.rete.matcher.{ReteBackendFactory, TimelyReteBackendFactory}

case class FunctionalOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = defaultTransformations,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false,
                             mode: ReteBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL) extends Options {

  override def withOptimizations(opts: Seq[Optimization]): FunctionalOptions =
    FunctionalOptions(
      opts,
      transformations,
      stopOnError,
      stopOnWarning,
      mode
    )

  override def withTransformations(trans: Seq[Transformation]): FunctionalOptions =
    FunctionalOptions(
      optimizations,
      trans,
      stopOnError,
      stopOnWarning,
      mode
    )
}

object FunctionalOptions {
  val defaultTransformations: Seq[Transformation] = Seq(
    //    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}