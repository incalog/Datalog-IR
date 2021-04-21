package inca

import inca.compiler.{CompiledModule, Compiler, Options}
import inca.runtime.Query.Match
import inca.runtime.{Database, EnginePool}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

import scala.jdk.CollectionConverters._

object Executor {
  case class Loaded(engine: AdvancedViatraQueryEngine, feed: Database, compiled: CompiledModule) {
    var lastTree: Diffable = _

    def output(pat: String): Seq[Match] = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      val mainMatcher = engine.getMatcher(mainSpec)
      mainMatcher.getAllMatches().asScala.toSeq
    }

    // use execute only once
    def execute[T <: Diffable](tree: T, pat: String): Seq[Match] = {
      val es = Diffable.load(tree)
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

  def loadAnalysis(code: String, langMetaInfo: LanguageMetaInfo): Loaded = {
    val options = Options(langMetaInfo)
    val compiled = Compiler.compileFun(code, options)
    val scope = new QueryScope(langMetaInfo)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    Loaded(engine, feed, compiled)
  }
}
