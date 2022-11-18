package inca.frontend.runner

import inca.compiler.CompiledModule
import inca.frontend.Constants.RelationName
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

trait Datalog[I <: Input, R <: Runner[I]] {
  protected def compiled: CompiledModule

  private val scope = new QueryScope(compiled.dataModel)
  protected val (engine, database) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  def runner(relName: RelationName): R
}

class IRDatalog(override val compiled: CompiledModule) extends Datalog[IRInput, IRRunner] {
  override def runner(relName: RelationName): IRRunner = new IRRunner(relName, compiled, engine, database)
}
