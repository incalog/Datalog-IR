package inca.frontend.souffle.executor

import inca.compiler.source.Source
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.compiler.SouffleOptions
import inca.frontend.souffle.lowering.SouffleInputReader
import inca.frontend.souffle.lowering.SouffleInputToEditscript
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.lowering.SouffleToNamedRelations
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax.RuleSignature
import inca.runtime.context.QueryScope
import inca.runtime.db.Database
import inca.runtime.db.DatabaseInput
import inca.runtime.EnginePool
import inca.runtime.Query
import inca.runtime.Query.Match
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuple
import scala.jdk.CollectionConverters.CollectionHasAsScala
import truechange.EditScript
import truediff.Diffable

object SouffleExecutor {
  case class Loaded(
      engine: AdvancedViatraQueryEngine,
      feed: Database,
      compiled: CompiledSouffleModule) {

    def output(matcher: Query.Matcher): Seq[Match] = {
      matcher.getAllMatches.asScala.toSeq
    }

    private def matcher(pat: String): Query.Matcher = {
      val mainSpec = compiled.psystemModule.patterns(pat)()
      engine.getMatcher(mainSpec)
    }

    def output(pat: String): Seq[Match] = {
      val mainMatcher = matcher(pat)
      output(mainMatcher)
    }

    // use execute only once
    def execute[T <: Diffable](pat: String, dir: String): Seq[Match] = {
      val patMatcher = matcher(pat)
      engine.delayUpdatePropagation(() => {
        loadInputs(dir)
      })
      output(patMatcher)
    }

    def execute[T <: Diffable](
        pat: String,
        inputs: Map[RuleSignature, String],
        delimiter: String = "\n"
      ): Seq[Match] = {
      val patMatcher = matcher(pat)
      engine.delayUpdatePropagation(() => {
        loadInputs(inputs, delimiter)
      })
      output(patMatcher)
    }

    private def loadInputs(dir: String): Unit = {
      val inputReader = getSouffleInputReader(dir)
      var dbInput: DatabaseInput = DatabaseInput.empty
      compiled.inputs.foreach { case (_, (sig, input)) =>
        val newInput = inputReader.compile(input, sig)
        dbInput = dbInput.combine(newInput)
      }
      engine.delayUpdatePropagation { () =>
        feed.processDatabaseInput(dbInput)
      }
    }

    def getSouffleInputReader(dir: String): SouffleInputReader =
      if (compiled.options.useEditScriptsForInput)
        new SouffleInputToEditscript(dir)
      else
        new SouffleToNamedRelations(dir)

    private def loadInputs(inputs: Map[RuleSignature, String], delimiter: String = "\t"): Unit = {
      val inputReader = getSouffleInputReader("EMPTY")
      var dbInput: DatabaseInput = DatabaseInput.empty
      inputs.foreach { case (sig, content) =>
        val rows = content.split("\n")
        val newInput = inputReader.compile(rows.toIterator, sig, delimiter)
        dbInput = dbInput.combine(newInput)
      }
      engine.delayUpdatePropagation { () =>
        feed.processDatabaseInput(dbInput)
      }
    }
  }

  def loadAnalysis(code: Source, options: SouffleOptions = SouffleOptions()): Loaded = {
    val ast = Parser.parse(code)
    val compiler = new SouffleToDatalogIR(options.useEditScriptsForInput)
    val compiled = compiler.compile("soufflemod", ast)
    println(compiled.ir)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, options.mode)
    Loaded(engine, feed, compiled)
  }
}
