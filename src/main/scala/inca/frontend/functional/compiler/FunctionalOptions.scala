package inca.frontend.functional.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.functional.compiler.FunctionalOptions.defaultTransformations

case class FunctionalOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = defaultTransformations,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options {

  override def withOptimizations(opts: Seq[Optimization]): FunctionalOptions =
    FunctionalOptions(
      opts,
      transformations,
      stopOnError,
      stopOnWarning
    )

  override def withTransformations(trans: Seq[Transformation]): FunctionalOptions =
    FunctionalOptions(
      optimizations,
      trans,
      stopOnError,
      stopOnWarning
    )
}

object FunctionalOptions {
  val defaultTransformations: Seq[Transformation] = Seq(
    //    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}