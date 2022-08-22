package inca.frontend.objectoriented.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.objectoriented.compiler.ObjectOptions.defaultTransformations

case class ObjectOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                         transformations: Seq[Transformation] = defaultTransformations,
                         stopOnError: Boolean = true,
                         stopOnWarning: Boolean = false) extends Options {

  override def withOptimizations(opts: Seq[Optimization]): ObjectOptions =
    ObjectOptions(
      opts,
      transformations,
      stopOnError,
      stopOnWarning
    )

  override def withTransformations(trans: Seq[Transformation]): ObjectOptions =
    ObjectOptions(
      optimizations,
      trans,
      stopOnError,
      stopOnWarning
    )
}

object ObjectOptions {
  val defaultTransformations: Seq[Transformation] = Seq(
    //    RemoveBodyOfUnusedDataConstructor,
    DeriveDemandPatterns,
    DemandTransformation)
}