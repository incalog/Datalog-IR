package inca.debugger

import inca.backend.ir.Datalog.Name
import inca.runtime.{Database, EnginePool, Query}
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

trait Debugger {
  // database
  val scope: QueryScope
  val (engine, db): (AdvancedViatraQueryEngine,Database) =
    EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  // debug state
  private val callStack: CallStack = new CallStack()

  def start(funName: Name, funArgs: Environment, initialSuspended: Suspended): Unit = {
    callStack.push(new CallFrame(funName, funArgs, initialSuspended))
  }

  protected def queryCall(spec: Query.Specification, args: Environment): Environment = {
    val matcher = engine.getMatcher(spec)
    spec.getPositionOfParameter()
    matcher.getAllMatches(null)
  }
}
