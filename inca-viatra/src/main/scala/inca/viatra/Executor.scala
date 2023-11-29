package inca.viatra

import inca.ir.CompiledModule
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, UnitRelation}
import inca.util.ScalaCompiler
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime.Query.Specification
import inca.viatra.runtime.{EnginePool, Query}
import inca.viatra.runtime.context.{DataModel, QueryScope}
import inca.viatra.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

object Executor extends IRExecutor:
  class Engine(val engine: AdvancedViatraQueryEngine, val feed: Database, module: PSystem.Module) extends ExecutorEngine:
    private def toQueryMatch(parameterNames: Seq[String], arity: Int, values: Seq[AnyRef], spec: Specification): Query.Match = {
      val params = parameterNames.zip(values).map { case (p, v) => spec.getPositionOfParameter(p) -> v }.toMap
      val arr = Seq.range(0, arity).map(params.getOrElse(_, null))
      Query.Match(spec, arr.toArray, isMutable = false)
    }

    override def read(rel: Relation): Relation =
      val spec = module.patterns(rel.name)()
      val matcher = spec.getMatcher(engine)

      import scala.jdk.CollectionConverters.*
      val parameterNames = matcher.getParameterNames.asScala.toSeq

      val output =
        if (rel.nonEmpty)
          rel.entries.flatMap { t =>
            val inputMatch = toQueryMatch(rel.parameterNames, parameterNames.size, rel.flattenEntry(t), spec)
            matcher.getAllMatches(inputMatch).asScala
          }
        else {
          val queryMatch = toQueryMatch(parameterNames, parameterNames.size, Seq(), spec)
          matcher.getAllMatches(queryMatch).asScala
        }
      Relation.fromMatches(rel.name, parameterNames, output.toSeq.map(_.toArray.toSeq).distinct)

    override def readAll(): Seq[Relation] =
      val pattern = module.patterns.keys.toSeq.sorted
      pattern.map(n => read(UnitRelation(n)))

    override def insert(edb: Relation): Unit =
      if (edb.entries.nonEmpty) {
        edb.entries.foreach { t =>
          val input = edb.flattenEntry(t)
          feed.insertExtensionalTuple(edb.name, Tuples.flatTupleOf(input: _*))
        }
      } else {
        feed.insertExtensionalTuple(edb.name, Tuples.flatTupleOf())
      }


  override def instantiate(m: CompiledModule): Engine =
    val code = GeneratePSystem.compileModules(Seq(m.lowered), true)
    val loadSource = s"$code;\n${m.name}"
    val compiler = new ScalaCompiler()
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(loadSource)

    val scope = new QueryScope(new DataModel())
    val (viatraEngine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    new Engine(viatraEngine, feed, psystemModule)

