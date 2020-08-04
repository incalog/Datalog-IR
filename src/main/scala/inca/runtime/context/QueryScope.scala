package inca.runtime.context

import inca.runtime.index.Index
import org.apache.log4j.Logger
import org.eclipse.viatra.query.runtime.api.ViatraQueryEngine
import org.eclipse.viatra.query.runtime.api.scope.{IEngineContext, IIndexingErrorListener}

class QueryScope(val langMetaInfo: LanguageMetaInfo, val additionalIndices: Seq[()=>Index]) extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {
  def this(languageMetaInfo: LanguageMetaInfo) =
    this(languageMetaInfo, Index.allAdditionalIndices)

  override def createEngineContext(engine: ViatraQueryEngine, errorListener: IIndexingErrorListener, logger: Logger): IEngineContext = {
    new EngineContext(this)
  }
}
