package inca.frontend.souffle.compiler

import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.Options
import org.eclipse.viatra.query.runtime.rete.matcher.{DRedReteBackendFactory, ReteBackendFactory, TimelyReteBackendFactory}

case class SouffleOptions(
    override val stopOnError: Boolean = true,
    override val stopOnWarning: Boolean = false,
    override val optimizations: Seq[Optimization] = Options.defaultOptimizations,
    override val transformations: Seq[Transformation] = Seq(),
    override val mode: ReteBackendFactory = DRedReteBackendFactory.INSTANCE,
    useEditScriptsForInput: Boolean = false)
    extends Options {

  override def withOptimizations(opts: Seq[Optimization]): SouffleOptions =
    this.copy(optimizations = opts)

  override def withTransformations(trans: Seq[Transformation]): SouffleOptions =
    this.copy(transformations = trans)

  override def withEngine(reteBackendFactory: ReteBackendFactory): Options =
    this.copy(mode = reteBackendFactory)
}
