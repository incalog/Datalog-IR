package inca.viatra

import inca.ir.CompiledModule
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation, RelationName, RelationUpdateListener, UnitRelation}
import inca.util.ScalaCompiler
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime.Query.Specification
import inca.viatra.runtime.{EnginePool, Query}
import inca.viatra.runtime.context.{DataModel, QueryScope}
import inca.viatra.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.{AdvancedViatraQueryEngine, IMatchUpdateListener}
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.{DRedReteBackendFactory, TimelyReteBackendFactory}

class Executor(backendFactory: IQueryBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL) extends IRExecutor:
  class Engine(val engine: AdvancedViatraQueryEngine, val feed: Database, module: PSystem.Module) extends ExecutorEngine:

    override def read(rel: Relation): ViatraRelation =
      val spec = module.patterns(rel.name)()
      val matcher = spec.getMatcher(engine)
      new ViatraRelation(rel, spec, matcher)

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

    override def remove(edb: Relation): Unit =
      if (edb.entries.nonEmpty) {
        edb.entries.foreach { t =>
          val input = edb.flattenEntry(t)
          feed.deleteExtensionalTuple(edb.name, Tuples.flatTupleOf(input: _*))
        }
      } else {
        feed.deleteExtensionalTuple(edb.name, Tuples.flatTupleOf())
      }

    override def addUpdateListener(up: RelationUpdateListener): Unit =
      val spec = module.patterns(up.rel.name)()
      val matcher = spec.getMatcher(engine)
      engine.addMatchUpdateListener(matcher, ViatraUpdateListener(up), false)

    override def removeUpdateListener(up: RelationUpdateListener): Unit =
      val spec = module.patterns(up.rel.name)()
      val matcher = spec.getMatcher(engine)
      engine.removeMatchUpdateListener(matcher, ViatraUpdateListener(up))

  override def instantiate(m: CompiledModule): Engine =
    val options = m.compilerOptions
    val code = GeneratePSystem.compileModules(Seq(m.lowered), options)
    val loadSource = s"$code;\n${m.name}"
    val compiler = new ScalaCompiler(options)
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(loadSource)

    val scope = new QueryScope(new DataModel())
    val (viatraEngine, feed) = EnginePool.loadEngineAndDatabase(scope, backendFactory)
    new Engine(viatraEngine, feed, psystemModule)

/** Rewrites matcher into Scala relation on-demand only */
class ViatraRelation(queryRel: Relation, spec: Query.Specification, matcher: Query.Matcher) extends Relation:
  import scala.jdk.CollectionConverters.*

  private var evaled: Boolean = false
  lazy val outputRel =
    evaled = true
    def toQueryMatch(parameterNames: Seq[String], arity: Int, values: Seq[AnyRef], spec: Specification): Query.Match = {
      val params = parameterNames.zip(values).map { case (p, v) => spec.getPositionOfParameter(p) -> v }.toMap
      val arr = Seq.range(0, arity).map(params.getOrElse(_, null))
      Query.Match(spec, arr.toArray, isMutable = false)
    }
    val output =
      if (queryRel.nonEmpty)
        queryRel.entries.flatMap { t =>
          val inputMatch = toQueryMatch(parameterNames, parameterNames.size, queryRel.flattenEntry(t), spec)
          matcher.getAllMatches(inputMatch).asScala
        }
      else {
        val queryMatch = toQueryMatch(parameterNames, parameterNames.size, Seq(), spec)
        matcher.getAllMatches(queryMatch).asScala
      }
    Relation.fromMatches(name, parameterNames, output.toSeq.map(_.toArray.toSeq).distinct)

  override type Tuple = Any
  override def name: RelationName = queryRel.name
  override def arity: Int = matcher.getParameterNames.size()
  override def parameterNames: Seq[String] = matcher.getParameterNames.asScala.toSeq

  override def size: Int = outputRel.size
  override def entries: Iterable[Tuple] = outputRel.entries
  override def unflattenEntry(entry: Seq[Any]): Any = outputRel.unflattenEntry(entry)
  override def matches: Iterable[Seq[Any]] = outputRel.matches

  override def toString: RelationName =
    val size = if (evaled) this.size.toString else "?"
    val entriesS =
      if (evaled)
        matches.map { e =>
          parameterNames.zip(e).map { case (name, value) =>
            s"$name: $value"
          }.mkString("(", ", ", ")")
        }.mkString("{", ", ", "}")
      else
        "?"
    s"${getClass.getSimpleName}(name: $name, size: $size, entries: $entriesS)"

case class ViatraUpdateListener(up: RelationUpdateListener) extends IMatchUpdateListener[Query.Match]:
  override def notifyAppearance(m: Query.Match): Unit = up.tupleAdded(up.rel.unflattenEntry(m.toArray.toSeq))
  override def notifyDisappearance(m: Query.Match): Unit = up.tupleRemoved(up.rel.unflattenEntry(m.toArray.toSeq))

