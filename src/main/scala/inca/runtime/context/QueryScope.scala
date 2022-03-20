package inca.runtime.context

import inca.runtime.index.dynamic.DynamicIndexFactory
import inca.runtime.index.Index
import org.apache.log4j.Logger
import org.eclipse.viatra.query.runtime.api.scope.IEngineContext
import org.eclipse.viatra.query.runtime.api.scope.IIndexingErrorListener
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine

class QueryScope(val langMetaInfo: DataModel, val dynamicIndices: Seq[DynamicIndexFactory])
    extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {
  def this(languageMetaInfo: DataModel) =
    this(languageMetaInfo, Index.allDynamicIndices)

  override def createEngineContext(
      engine: ViatraQueryEngine,
      errorListener: IIndexingErrorListener,
      logger: Logger
    ): IEngineContext = {
    new EngineContext(this)
  }
}
