package inca.frontend.constraint.executor

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.CompiledModule
import inca.compiler.Compiler
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import inca.runtime.EnginePool
import inca.runtime.Query
import inca.runtime.Query.Match
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import scala.jdk.CollectionConverters._
import truediff.Diffable

object ConstraintExecutor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
    var lastTree: Diffable = _
    var lastInput: Tuple = _

    def output(pat: String, tuple: Tuple = null): Seq[Match] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      if (tuple == null)
        mainMatcher.getAllMatches.asScala.toSeq
      else {
        val arity = mainMatcher.getParameterNames.size()
        val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
        val inputMatch = Query.Match(mainSpec, inputSeq, isMutable = false)
        val outputMatches = mainMatcher.getAllMatches(inputMatch).asScala.toSeq
        outputMatches
      }
    }

    // use execute only once
    def execute[T <: Diffable](tree: T, pat: String, input: Tuple = null): Seq[Match] = {
      val es = tree.loadEdits
      lastTree = tree
      lastInput = input
      feed.processEditScript(es)
      if (input != null)
        feed.insert(demandPatternExtensionalPrefix + pat, input)
      output(pat, input)
    }

    // use update after calling execute once to update analysis based on new tree
    def update[T <: Diffable](newTree: T, pat: String, input: Tuple = null): Seq[Match] = {
      val (es, updatedTree) = lastTree.compareTo(newTree)
      lastTree = updatedTree
      feed.processEditScript(es)

      if (lastInput != null)
        feed.delete(demandPatternExtensionalPrefix + pat, input)
      if (input != null)
        feed.insert(demandPatternExtensionalPrefix + pat, input)
      lastInput = input

      output(pat, input)
    }
  }

  def loadAnalysis(code: String, options: ConstraintOptions = ConstraintOptions()): Loaded = {
    val compiled = Compiler.compileConstraint(code, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, options.mode)
    Loaded(engine, feed, compiled)
  }
}
