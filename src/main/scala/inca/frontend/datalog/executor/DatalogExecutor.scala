package inca.frontend.datalog.executor

import inca.frontend.datalog.compiler.{CompiledDatalogModule, DatalogOptions}
import inca.runtime.EnginePool
import inca.runtime.Query.Match
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import inca.util.Scala.ScalaCompiler
import org.eclipse.collections.api.multimap.list.MutableListMultimap
import org.eclipse.collections.impl.factory.Multimaps
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.{Tuple, Tuples}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.{EditScriptBuffer, URI}
import truediff.Diffable

import scala.jdk.CollectionConverters._

object DatalogExecutor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledDatalogModule) {

    lazy val scalaCompiler: ScalaCompiler = new ScalaCompiler
    lazy val loadedPsystemModule: String = scalaCompiler.define {
      import scala.meta._
      q"object O {..${compiled.psystemSource.stats}}".syntax
    }

    def output(pat: String): Seq[Match] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      mainMatcher.getAllMatches().asScala.toSeq
    }

    def load(term: meta.Term): Diffable = {
      engine.delayUpdatePropagation { () =>
        val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${term.syntax}}"
        val v = scalaCompiler.compileAndLoadScala[Diffable](syntax)
        val edits = v.loadEdits
        feed.processEditScript(edits)
        v
      }
    }

    def unload(any: Any): Unit = any match {
      case v: Diffable =>
        val edits = new EditScriptBuffer
        v.unloadUnassigned(edits)
        feed.processEditScript(edits.toEditScript)
      case _ => // nothing
    }

    def vals(anys: Seq[Any]): Seq[Any] = anys.map {
      case v: Diffable => v.uri
      case v => v
    }

    private val insertedURIs: MutableListMultimap[URI, (String, Tuple, Int)] = Multimaps.mutable.list.empty()

    def insert(extensionalRel: String, anys: Any*): Unit = {
      val values = vals(anys)
      val tup = Tuples.flatTupleOf(values:_*)
      values.zipWithIndex.foreach {
        case (uri: URI, ix) => insertedURIs.put(uri, (extensionalRel, tup, ix))
        case (a, _) => feed.loadPrimitive(a)
      }
      feed.insertExtensionalTuple(extensionalRel, tup)
    }

    def remove(extensionalRel: String, anys: Any*): Unit = {
      val values = vals(anys)
      val tup = Tuples.flatTupleOf(values:_*)
      values.foreach {
        case uri: URI => insertedURIs.removeAll(uri)
        case a => feed.unloadPrimitive(a)
      }
      feed.deleteExtensionalTuple(extensionalRel, tup)
    }

    def replace(was: Diffable, now: meta.Term): Diffable = {
      engine.delayUpdatePropagation { () =>
        val syntax = s"{import ${loadedPsystemModule}.${compiled.name}._; ${now.syntax}}"
        val v = scalaCompiler.compileAndLoadScala[Diffable](syntax)
        val (edits, replaced) = was.compareTo(v)
        feed.processEditScript(edits)
        if (was.uri != replaced.uri) {
          insertedURIs.get(was.uri).asScala.foreach { case (rel, tup, ix) =>
            feed.deleteExtensionalTuple(rel, tup)
            val elements = tup.getElements
            elements(ix) = v.uri
            feed.insertExtensionalTuple(rel, Tuples.flatTupleOf(elements: _*))
          }
        }
        replaced
      }
    }
  }

  def loadDatalog(code: String): Loaded = {
    val options = DatalogOptions()
    val compiled = CompiledDatalogModule(code, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
}
