package inca.backend.indices

import inca.backend.virtual.VirtualIndex
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContext

import scala.collection.mutable
import scala.jdk.CollectionConverters._


// we pass the virtualindices because we want them to be configurable
case class EngineContext(scope: QueryScope, engine: AdvancedViatraQueryEngine, virtualIndices: Map[String, VirtualIndex]) extends IEngineContext {
  // how do we populate the supertype map?
  val indices: Indices = new Indices(engine, (mutable.Map() ++ virtualIndices).asJava)
  val runtimeCtx = new TFRuntimeContext(indices, new MetaContext(scope.supertypes, scope.links))

  override def getBaseIndex: Indices = indices
  override def getQueryRuntimeContext: IQueryRuntimeContext = runtimeCtx
  override def dispose(): Unit = {
    indices.dispose()
  }
}
