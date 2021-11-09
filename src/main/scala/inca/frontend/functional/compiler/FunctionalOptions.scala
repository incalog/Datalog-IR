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
                             engine: ReteBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options {

  override def withOptimizations(opts: Seq[Optimization]): FunctionalOptions =
    FunctionalOptions(
      opts,
      transformations,
      engine,
      stopOnError,
      stopOnWarning
    )

  override def withTransformations(trans: Seq[Transformation]): FunctionalOptions =
    FunctionalOptions(
      optimizations,
      trans,
      engine,
      stopOnError,
      stopOnWarning
    )

  override def withEngine(eng: ReteBackendFactory): FunctionalOptions =
    FunctionalOptions(optimizations, transformations, eng, stopOnError, stopOnWarning)
}

object FunctionalOptions {
  val defaultTransformations: Seq[Transformation] = Seq(
    //    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}