package inca.frontend.objectoriented.runner

import inca.compiler.CompiledModule
import inca.frontend.objectoriented.compiler.CompiledObjectModule
import inca.frontend.runner.{RelationName, RunnerFactory}
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory


class ObjectOrientedRunnerFactory(override val compiled: CompiledObjectModule)
  extends RunnerFactory[ObjectOrientedInput, ObjectOrientedRunner] {

  override def runner(relName: RelationName): ObjectOrientedRunner =
    new ObjectOrientedRunner(relName, compiled, engine, database)

  def runner(className: String, methodName: String): ObjectOrientedRunner =
    new ObjectOrientedRunner(className + "$" + methodName, compiled, engine, database)
}
