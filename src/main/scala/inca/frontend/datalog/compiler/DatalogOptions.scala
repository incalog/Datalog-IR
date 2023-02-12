package inca.frontend.datalog.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import inca.compiler.Options.defaultOptimizations
import inca.frontend.functional.compiler.FunctionalOptions.defaultTransformations
import org.eclipse.viatra.query.runtime.rete.matcher.ReteBackendFactory
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

case class DatalogOptions(
    optimizations: Seq[Optimization] = defaultOptimizations,
    transformations: Seq[Transformation] = defaultTransformations,
    _mode: ReteBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL,
    stopOnError: Boolean = true,
    stopOnWarning: Boolean = false)
    extends Options {
  override def withOptimizations(opts: Seq[Optimization]): Options =
    DatalogOptions(opts, transformations, mode, stopOnError, stopOnWarning)

  override def withTransformations(trans: Seq[Transformation]): Options =
    DatalogOptions(optimizations, trans, mode, stopOnError, stopOnWarning)

  override def mode: ReteBackendFactory = _mode

  override def withEngine(reteBackendFactory: ReteBackendFactory): Options =
    DatalogOptions(optimizations, transformations, reteBackendFactory, stopOnError, stopOnWarning)
}

object DatalogOptions {
  val defaultTransformations: Seq[Transformation] = Seq()
}
