package inca.runtime.context

import inca.runtime.index.DynamicKey
import inca.runtime.index.dynamic.DynamicIndex
import org.apache.log4j.Logger
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.{IEngineContext, IIndexingErrorListener}

case class QueryScope(langMetaInfo: LanguageMetaInfo, dynamicIndices: Map[DynamicKey, DynamicIndex]) extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {
  override def createEngineContext(engine: ViatraQueryEngine, errorListener: IIndexingErrorListener, logger: Logger): IEngineContext = {
    new EngineContext(this)
  }
}
