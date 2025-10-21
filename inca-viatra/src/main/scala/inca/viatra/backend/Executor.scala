package inca.viatra.backend

import inca.foreign.scala.ir.primitive.ScalaInca.cleanString
import inca.ir.CompiledUnit
import inca.ir.execution.*
import inca.util.ScalaCompiler
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime.Query.Specification
import inca.viatra.runtime.context.{DataModel, QueryScope}
import inca.viatra.runtime.db.Database
import inca.viatra.runtime.{EnginePool, Query}
import org.apache.log4j.{BasicConfigurator, Level}
import org.eclipse.viatra.query.runtime.api.{AdvancedViatraQueryEngine, IMatchUpdateListener}
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.eclipse.viatra.query.runtime.util.ViatraQueryLoggingUtil

object Executor:
  def initializeLogging(): Unit =
    BasicConfigurator.configure()
    setLogLevel(Level.OFF)

  def enableDebugLogging(): Unit =
    setLogLevel(Level.DEBUG)

  def disableLogging(): Unit =
    setLogLevel(Level.OFF)

  private def setLogLevel(level: Level): Unit =
    ViatraQueryLoggingUtil.getDefaultLogger.setLevel(level)

class Executor(backendFactory: IQueryBackendFactory = TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL) extends IRExecutor:
  override val name: String = "Viatra"

  class Engine(val engine: AdvancedViatraQueryEngine, val feed: Database, val module: PSystem.Module) extends ExecutorEngine:

    override def measure(rel: Relation): Long =
      val spec = module.patterns(cleanString(rel.name))()
      val start = System.nanoTime()
      val matcher = spec.getMatcher(engine)
      System.nanoTime() - start

    override def read(rel: Relation): ViatraRelation =
      val spec = module.patterns(cleanString(rel.name))()
      val matcher = spec.getMatcher(engine)
      new ViatraRelation(rel, spec, matcher)

    override def readAll(): Seq[Relation] =
      val pattern = module.patterns.keys.toSeq.sorted
      pattern.map(n => read(UnitRelation(n)))

    /**
     * Convert ADT values to their corresponding ADT instances. We only have access to the ADT classes, once PSystem is
     * loaded.
     */
    private def transformADT(dataName: String, caseName: String, args: Seq[Any]): Any =
      // use reflection to create an ADT instance
      val moduleClass = module.getClass
      val adtClass = moduleClass.getDeclaredClasses.find(_.getSimpleName == caseName) match
        case Some(cls) => cls
        case _ => throw IllegalArgumentException(s"Unknown ADT with name $caseName")
      // we assume all our ADTs only have a single constructor, the primary constructor
      val adtConstructor = adtClass.getDeclaredConstructors.head
      adtConstructor.setAccessible(true)
      adtConstructor.newInstance((module +: args)*)

    private def viatrafyTupleEntry(v: Any): Any =
      transformEDBInput(v)(identity, identity, identity, transformADT)
    
    override def insert(edb: Relation): Unit =
      edb.entries.foreach { t =>
        val input = edb.flattenEntry(t).map(viatrafyTupleEntry)
        feed.insertExtensionalTuple(edb.name, Tuples.flatTupleOf(input*))
      }

    override def remove(edb: Relation): Unit =
      edb.entries.foreach { t =>
        val input = edb.flattenEntry(t)
        feed.deleteExtensionalTuple(edb.name, Tuples.flatTupleOf(input*))
      }

    override def addUpdateListener(up: RelationUpdateListener): Unit =
      val spec = module.patterns(cleanString(up.rel.name))()
      val matcher = spec.getMatcher(engine)
      engine.addMatchUpdateListener(matcher, ViatraUpdateListener(up), false)

    override def removeUpdateListener(up: RelationUpdateListener): Unit =
      val spec = module.patterns(cleanString(up.rel.name))()
      val matcher = spec.getMatcher(engine)
      engine.removeMatchUpdateListener(matcher, ViatraUpdateListener(up))

  override def instantiate(m: CompiledUnit): Engine =
    instantiate(m, new DataModel())

  def instantiate(m: CompiledUnit, dataModel: DataModel): Engine =
    val options = m.compilerOptions
    val code = GeneratePSystem.compileModules(m.compiled, options)
    val loadSource = s"$code;\n${m.name}"
    val compiler = new ScalaCompiler(options)
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(loadSource)

    val scope = new QueryScope(dataModel)
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
        val scala = matcher.getAllMatches.asScala
        scala
      }
    Relation.from(name, parameterNames, output.toSeq.map(_.toArray.toSeq).distinct)

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

