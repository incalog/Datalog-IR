package inca.runtime

import java.lang.ref.WeakReference
import java.util

import org.eclipse.viatra.query.runtime.api._
import org.eclipse.viatra.query.runtime.api.scope.QueryScope
import org.eclipse.viatra.query.runtime.exception.ViatraQueryException
import org.eclipse.viatra.query.runtime.matchers.backend.{IQueryBackendFactory, QueryEvaluationHint}
import org.eclipse.viatra.query.runtime.matchers.context.IQueryBackendContext
import org.eclipse.viatra.query.runtime.matchers.psystem.queries.PQuery

import scala.jdk.CollectionConverters._

object EnginePool {
  private val engineMap: util.Map[QueryScope, WeakReference[AdvancedViatraQueryEngine]] = new util.WeakHashMap

  def getMatcher[Matcher <: ViatraQueryMatcher[_]](specification: IQuerySpecification[Matcher], scope: QueryScope, backendFactory: IQueryBackendFactory): Matcher = try {
    val engineReference = EnginePool.engineMap.get(scope)

    val engine =
      if (engineReference != null && engineReference.get != null) {
        engineReference.get
      } else {
        val options = ViatraQueryEngineOptions.defineOptions
          .withDefaultBackend(backendFactory)
          .withDefaultCachingBackend(backendFactory)
          .withDefaultSearchBackend(DummySearchBackendFactory).build
        val e = AdvancedViatraQueryEngine.createUnmanagedEngine(scope, options)
        EnginePool.engineMap.put(scope, new WeakReference(e))
        e
      }
    engine.getMatcher(specification, null)
  } catch {
    case e: ViatraQueryException =>
      e.printStackTrace()
      null.asInstanceOf[Matcher]
  }

  def getEngines: util.Collection[WeakReference[AdvancedViatraQueryEngine]] =
    EnginePool.engineMap.values

  def disposeAllEngines(): Unit = {
    for (ref <- EnginePool.engineMap.values.asScala) {
      val engine = ref.get
      if (engine != null) {
        System.err.println("Disposing engine " + engine)
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