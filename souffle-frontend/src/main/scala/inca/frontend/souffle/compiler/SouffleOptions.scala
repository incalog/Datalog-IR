package inca.frontend.souffle.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.eclipse.viatra.query.runtime.rete.matcher.ReteBackendFactory

case class SouffleOptions(
    override val stopOnError: Boolean = true,
    override val stopOnWarning: Boolean = false,
    override val optimizations: Seq[Optimization] = Options.defaultOptimizations,
    override val transformations: Seq[Transformation] = Seq(),
    override val mode: ReteBackendFactory = DRedReteBackendFactory.INSTANCE,
    useEditScriptsForInput: Boolean = false)
    extends Options {

  override def withOptimizations(opts: Seq[Optimization]): SouffleOptions =
    SouffleOptions(stopOnError, stopOnWarning, opts, transformations, mode, useEditScriptsForInput)

  override def withTransformations(trans: Seq[Transformation]): SouffleOptions =
    SouffleOptions(stopOnError, stopOnWarning, optimizations, trans, mode, useEditScriptsForInput)
}
