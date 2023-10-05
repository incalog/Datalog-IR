package inca.runtime

import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.*
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.matchers.backend.{IQueryBackendFactory, QueryEvaluationHint}
import org.eclipse.viatra.query.runtime.matchers.context.IQueryBackendContext
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery

import java.lang.ref.WeakReference
import java.util
import scala.jdk.CollectionConverters.*

object EnginePool {
  private val engineMap: util.Map[QueryScope, WeakReference[AdvancedViatraQueryEngine]] = new util.WeakHashMap

  def loadEngine(scope: QueryScope, backendFactory: IQueryBackendFactory): AdvancedViatraQueryEngine = {
    val engineReference = EnginePool.engineMap.get(scope)
    val engine =
      if (engineReference != null && engineReference.get != null) {
        engineReference.get
      } else {
        val options = ViatraQueryEngineOptions.defineOptions
          .withDefaultBackend(backendFactory)
          .withDefaultCachingBackend(backendFactory)
          .withDefaultSearchBackend(DummySearchBackendFactory)
//          .withDefaultHint(new QueryEvaluationHint(Collections.singletonMap(ReteHintOptions.cacheOutputOfEvaluatorsByDefault, false), BackendRequirement.UNSPECIFIED))
          .build
        val e = AdvancedViatraQueryEngine.createUnmanagedEngine(scope, options)
        EnginePool.engineMap.put(scope, new WeakReference(e))
        e
      }
    engine
  }

  def loadDatabase(scope: QueryScope, backendFactory: IQueryBackendFactory): Database =
    loadEngine(scope, backendFactory).getBaseIndex.asInstanceOf[Database]

  def loadEngineAndDatabase(scope: QueryScope, backendFactory: IQueryBackendFactory): (AdvancedViatraQueryEngine, Database) = {
    val engine = loadEngine(scope, backendFactory)
    val database = engine.getBaseIndex.asInstanceOf[Database]
    (engine, database)
  }

  def loadQuery(specification: Query.Specification,
                scope: QueryScope,
                backendFactory: IQueryBackendFactory): Query.Matcher = {
    val engine = loadEngine(scope, backendFactory)
    engine.getMatcher(specification, null)
  }

  def disposeAllEngines(): Unit = {
    for (ref <- EnginePool.engineMap.values.asScala) {
      val engine = ref.get
      if (engine != null) {
//        System.err.println("Disposing engine " + engine)
        engine.dispose()
      }
    }
    EnginePool.engineMap.clear()
  }

  object DummySearchBackendFactory extends IQueryBackendFactory {
    override def create(context: IQueryBackendContext): Null = null
    override def getBackendClass: Null = null
    override def calculateRequiredCapability(query: PQuery, hint: QueryEvaluationHint): Null = null
    override def isCaching = false
  }
}