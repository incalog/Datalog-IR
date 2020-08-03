package inca.runtime.context

import inca.runtime.index.Index
import org.apache.log4j.Logger
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.{IEngineContext, IIndexingErrorListener}

case class QueryScope(langMetaInfo: LanguageMetaInfo, additionalIndices: Seq[Index]) extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {
  override def createEngineContext(engine: ViatraQueryEngine, errorListener: IIndexingErrorListener, logger: Logger): IEngineContext = {
    new EngineContext(this)
  }
}
