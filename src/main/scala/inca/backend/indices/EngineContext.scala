package inca.backend.indices

import inca.backend.virtual.VirtualIndex
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContext

import scala.jdk.CollectionConverters._


// we pass the virtualindices because we want them to be configurable
case class EngineContext(scope: QueryScope, engine: AdvancedViatraQueryEngine, virtualIndices: Seq[VirtualIndex]) extends IEngineContext {


  private def scala2JavaNestedMap(map: Map[String, Set[String]]): java.util.Map[String, java.util.Set[String]] = {
    val res = new java.util.HashMap[String, java.util.Set[String]]()
    map.foreach { case (key, set) =>
      res.put(key, set.asJava)
    }
    res
  }

  val indices: Indices = new Indices(engine, scala2JavaNestedMap(scope.langMetaInfo.subtypes), scala2JavaNestedMap(scope.langMetaInfo.supertypes), virtualIndices.asJava)
  val runtimeCtx = new TFRuntimeContext(indices, new MetaContext(scope.langMetaInfo))

  override def getBaseIndex: Indices = indices
  override def getQueryRuntimeContext: IQueryRuntimeContext = runtimeCtx
  override def dispose(): Unit = {
    indices.dispose()
  }
}
