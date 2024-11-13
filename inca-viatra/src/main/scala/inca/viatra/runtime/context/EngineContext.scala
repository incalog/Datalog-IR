package inca.viatra.runtime.context

import inca.viatra.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.scope.{IBaseIndex, IEngineContext}
import org.eclipse.viatra.query.runtime.matchers.context.IQueryRuntimeContext

class EngineContext(scope: QueryScope) extends IEngineContext:

  var database = new Database(scope.langMetaInfo, scope.dynamicIndices, new MetaContext(scope.langMetaInfo))

  override def getBaseIndex: IBaseIndex = database

  override def getQueryRuntimeContext: IQueryRuntimeContext = database

  override def dispose(): Unit = {
    database = null
  }
