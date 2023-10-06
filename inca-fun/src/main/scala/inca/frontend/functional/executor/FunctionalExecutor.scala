package inca.frontend.functional.executor

import inca.frontend.functional.compile.GenerateDatalog.extensionalRelationName
import inca.frontend.functional.compile.{CompiledFunctionalModule, GenerateDatalog}
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.db.{DBValue, Database, DatabaseInspector}
import inca.runtime.{EnginePool, Query, Relation}
import inca.util.ScalaCompiler
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript
import truediff.Diffable
import inca.frontend.functional.syntax.*

import scala.jdk.CollectionConverters.*

object FunctionalExecutor:
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledFunctionalModule):
    //lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler

    def output(pat: String, tuple: Tuple): Relation = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      val arity = mainMatcher.getParameterNames.size()
      Relation.fromMatcher(mainMatcher).slice(tuple.getSize, arity)

      /*val arity = mainMatcher.getParameterNames.size()
      val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
      val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)
      val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.map { m =>
        m.toArray.slice(tuple.getSize, arity).toSeq
      }.toSeq
      val paramNames = mainMatcher.getParameterNames.subList(tuple.getSize, arity)
      Relation.from(pat, paramNames.asScala.toSeq, outputMatches)*/
    }

    def execute(main: String, args: Seq[Any]): Relation = {
      // TODO: Support input arguments
      val input = Tuples.flatTupleOf(args:_*)
      feed.insertExtensionalTuple(extensionalRelationName(main), input)
      output(main, input)
    }

  def loadFunction(compiled: CompiledFunctionalModule): Loaded = {
    // TODO: use correct DataModel
    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }

  def compileFunction(code: String): CompiledFunctionalModule = {
    val module = Parser.parseModule(code)
    CompiledFunctionalModule(module)
  }
