package inca.frontend.constraint.executor

import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.Query.Match
import inca.runtime.context.QueryScope
import inca.runtime.EnginePool
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

import scala.jdk.CollectionConverters._

object ConstraintExecutor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
    var lastTree: Diffable = _

    def output(pat: String): Seq[Match] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      mainMatcher.getAllMatches().asScala.toSeq
    }

    // use execute only once
    def execute[T <: Diffable](tree: T, pat: String): Seq[Match] = {
      val es = tree.loadEdits
      lastTree = tree
      feed.processEditScript(es)
      output(pat)
    }

    // use update after calling execute once to update analysis based on new tree
    def update[T <: Diffable](newTree: T, pat: String): Seq[Match] = {
      val (es, updatedTree) = lastTree.compareTo(newTree)
      lastTree = updatedTree
      feed.processEditScript(es)
      output(pat)
    }
  }

  def loadAnalysis(code: String): Loaded = {
    val options = ConstraintOptions()
    val compiled = Compiler.compileConstraint(code, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
}
