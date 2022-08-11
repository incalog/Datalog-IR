package inca.frontend.objectoriented.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.objectoriented.compiler.ObjectOrientedOptions.defaultTransformations

case class ObjectOrientedOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                             transformations: Seq[Transformation] = defaultTransformations,
                             stopOnError: Boolean = true,
                             stopOnWarning: Boolean = false) extends Options {
  override def withOptimizations(opts: Seq[Optimization]): ObjectOrientedOptions =
    ObjectOrientedOptions(
      opts,
      transformations,
      stopOnError,
      stopOnWarning
    )

  override def withTransformations(trans: Seq[Transformation]): ObjectOrientedOptions =
    ObjectOrientedOptions(
      optimizations,
      trans,
      stopOnError,
      stopOnWarning
    )
}

object ObjectOrientedOptions {
  val defaultTransformations: Seq[Transformation] = Seq(
    //    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}