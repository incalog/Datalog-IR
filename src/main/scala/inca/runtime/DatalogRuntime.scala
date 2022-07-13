package inca.runtime

import inca.compiler.CompiledModule
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine

case class DatalogRuntime(
    engine: AdvancedViatraQueryEngine,
    db: Database,
    compiled: CompiledModule) {
  // TODO add methods to get answers to queries
  // TODO add methods to insert input
}
