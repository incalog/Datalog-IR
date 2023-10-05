package inca.frontend.functional.executor

import inca.frontend.functional.compile.CompiledFunctionalModule
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.db.{DBValue, Database, DatabaseInspector}
import inca.runtime.{EnginePool, Query, Relation}
import inca.util.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript
import truediff.Diffable

import scala.jdk.CollectionConverters.*

object FunctionalExecutor:
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledFunctionalModule):
    //lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

    def output(pat: String, tuple: Tuple): Relation = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      val arity = mainMatcher.getParameterNames.size()
      val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
      val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)
      val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
        m.toArray.slice(tuple.getSize, arity).toSeq
      }.toSeq
      val paramNames = mainMatcher.getParameterNames.subList(tuple.getSize, arity)
      Relation.from(pat, paramNames.asScala.toSeq, outputMatches)
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      // TODO: Support input arguments
      // feed.insertExtensionalTuple(demandPatternExtensionalPrefix + main, tuple)
      output(main, Tuples.flatTupleOf())
    }

  def loadFunction(compiled: CompiledFunctionalModule): Loaded = {
    // TODO: use correct DataModel
    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
