package inca.runtime.context

import inca.runtime.index.VirtualIndex
import org.apache.log4j.Logger
import org.eclipse.viatra.query.runtime.api.scope.{IEngineContext, IIndexingErrorListener}
import org.eclipse.viatra.query.runtime.api.{AdvancedViatraQueryEngine, ViatraQueryEngine}

class QueryScope(_langMetaInfo: LanguageMetaInfo, _virtualIndices: Seq[VirtualIndex]) extends org.eclipse.viatra.query.runtime.api.scope.QueryScope {

  val langMetaInfo: LanguageMetaInfo = _langMetaInfo
  val virtualIndices: Seq[VirtualIndex] = _virtualIndices

  var engineContext: EngineContext = _
  lazy val getEngineContext: EngineContext = engineContext

  override def createEngineContext(engine: ViatraQueryEngine, errorListener: IIndexingErrorListener, logger: Logger): IEngineContext = {
    if (this.engineContext == null) {
      this.engineContext = new EngineContext(this, engine.asInstanceOf[AdvancedViatraQueryEngine], virtualIndices)
    }
    this.engineContext
  }
}
