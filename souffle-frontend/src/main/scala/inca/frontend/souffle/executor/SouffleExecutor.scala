package inca.frontend.souffle.executor

import inca.backend.transform.magic.demand.DemandTransformation.demandPatternExtensionalPrefix
import inca.compiler.source.Source
import inca.compiler.Options
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.SouffleInputToEditscript
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax.RuleSignature
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import inca.runtime.EnginePool
import inca.runtime.Query
import inca.runtime.Query.Match
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import scala.jdk.CollectionConverters.CollectionHasAsScala
import truechange.Edit
import truechange.EditScript
import truediff.Diffable

object SouffleExecutor {
  case class Loaded(
      engine: AdvancedViatraQueryEngine,
      feed: Database,
      compiled: CompiledSouffleModule) {

    def output(matcher: Query.Matcher, tuple: Tuple): Seq[Match] = {
      if (tuple == null)
        matcher.getAllMatches.asScala.toSeq
      else {
        val arity = matcher.getParameterNames.size()
        val inputSeq = tuple.getElements ++ (for (_ <- 0 until (arity - tuple.getSize)) yield null)
        val inputMatch = Query.Match(
          matcher.getSpecification.asInstanceOf[Query.Specification],
          inputSeq,
          isMutable = false
        )
        val outputMatches = matcher.getAllMatches(inputMatch).asScala.toSeq
        outputMatches
      }
    }

    private def matcher(pat: String): Query.Matcher = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      engine.getMatcher(mainSpec)
    }

    def output(pat: String, tuple: Tuple): Seq[Match] = {
      val mainMatcher = matcher(pat)
      output(mainMatcher, tuple)
    }

    // use execute only once
    def execute[T <: Diffable](pat: String, dir: String, input: Tuple): Seq[Match] = {
      val patMatcher = matcher(pat)
      engine.delayUpdatePropagation(() => {
        loadInputs(dir)
        if (input != null)
          feed.insert(demandPatternExtensionalPrefix + pat, input)
      })
      output(patMatcher, input)
    }

    def execute[T <: Diffable](
        pat: String,
        inputs: Map[RuleSignature, String],
        delimiter: String = "\n",
        input: Tuple
      ): Seq[Match] = {
      val patMatcher = matcher(pat)
      engine.delayUpdatePropagation(() => {
        loadInputs(inputs, delimiter)
        if (input != null)
          feed.insert(demandPatternExtensionalPrefix + pat, input)
      })
      output(patMatcher, input)
    }

    private def loadInputs(dir: String): Unit = {
      val inputCompiler = new SouffleInputToEditscript(dir)
      var edits: Seq[Edit] = Seq()
      compiled.inputs.foreach { case (_, (sig, input)) =>
        val es = inputCompiler.compile(input, sig)
        edits ++= es.edits
      }
      engine.delayUpdatePropagation(() => feed.processEditScript(EditScript(edits)))
    }

    private def loadInputs(inputs: Map[RuleSignature, String], delimiter: String = "\t"): Unit = {
      val inputCompiler = new SouffleInputToEditscript("EMPTY")
      var edits: Seq[Edit] = Seq()
      inputs.foreach { case (sig, content) =>
        val rows = content.split("\n")
        val es = inputCompiler.compile(rows.toIterator, sig, delimiter)
        edits ++= es.edits
      }
      engine.delayUpdatePropagation(() => feed.processEditScript(EditScript(edits)))
    }
  }

  def loadAnalysis(code: Source, options: Options = Options()): Loaded = {
    val ast = Parser.parse(code)
    val compiler = new SouffleToDatalogIR
    val compiled = compiler.compile("soufflemod", ast)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, options.mode)
    Loaded(engine, feed, compiled)
  }
}
