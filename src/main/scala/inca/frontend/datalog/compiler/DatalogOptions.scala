package inca.frontend.datalog.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.functional.compiler.FunctionalOptions.defaultTransformations

case class DatalogOptions(optimizations: Seq[Optimization] = defaultOptimizations,
                          transformations: Seq[Transformation] = defaultTransformations,
                          stopOnError: Boolean = true,
                          stopOnWarning: Boolean = false) extends Options {
  override def withOptimizations(opts: Seq[Optimization]): Options =
    DatalogOptions(opts, transformations, stopOnError, stopOnWarning)

  override def withTransformations(trans: Seq[Transformation]): Options =
    DatalogOptions(optimizations, trans, stopOnError, stopOnWarning)
}

object DatalogOptions {
  val defaultTransformations: Seq[Transformation] = Seq()
}
