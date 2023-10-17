package inca.viatra

import inca.ir.CompiledModule
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation}
import inca.util.ScalaCompiler
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.{DataModel, QueryScope}
import inca.viatra.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

object Executor extends IRExecutor:
  class Engine(val engine: AdvancedViatraQueryEngine, val feed: Database, module: PSystem.Module) extends ExecutorEngine:
    override def read(rel: String): Relation =
      val spec = module.patterns(rel)()
      val matcher = engine.getMatcher(spec)

      import scala.jdk.CollectionConverters.*
      val res = Relation.fromMatches(
        matcher.getPatternName,
        matcher.getParameterNames.asScala.toList,
        matcher.getAllMatchArrays.map(_.toSeq))
      res

    override def readAll(): Seq[Relation] =
      val pattern = module.patterns.keys.toSeq.sorted
      pattern.map(n => read(n))

    override def insertAll(edb: String, tuples: Iterable[Seq[Any]]): Unit =
      for (tuple <- tuples) {
        val input = Tuples.flatTupleOf(tuple:_*)
        feed.insertExtensionalTuple(edb, input)
      }

  override def instantiate(m: CompiledModule): Engine =
    val code = GeneratePSystem.compileModules(Seq(m.lowered))
    val loadSource = s"$code;\n${m.name}"
    val compiler = new ScalaCompiler()
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(loadSource)

    val scope = new QueryScope(new DataModel())
    val (viatraEngine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    new Engine(viatraEngine, feed, psystemModule)

