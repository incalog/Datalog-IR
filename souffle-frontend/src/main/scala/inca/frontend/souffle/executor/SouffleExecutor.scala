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
      val input = loadInputs(dir, compiled)
      engine.delayUpdatePropagation(() => {
        feed.processDatabaseInput(input)
      })
      output(patMatcher)
    }

    def execute[T <: Diffable](
        pat: String,
        inputs: Map[RuleSignature, String],
        delimiter: String = "\n"
      ): Seq[Match] = {
      val patMatcher = matcher(pat)
      val input = loadInputs(inputs, compiled, delimiter)
      engine.delayUpdatePropagation(() => {
        feed.processDatabaseInput(input)
      })
      output(patMatcher)
    }
  }

  def getSouffleInputReader(dir: String, compiled: CompiledSouffleModule): SouffleInputReader =
    if (compiled.options.useEditScriptsForInput)
      new SouffleInputToEditscript(dir)
    else
      new SouffleToNamedRelations(dir)

  def loadInputs(dir: String, compiled: CompiledSouffleModule): DatabaseInput = {
    val inputReader = getSouffleInputReader(dir, compiled)
    var dbInput: DatabaseInput = DatabaseInput.empty
    compiled.inputs.foreach { case (_, (sig, input)) =>
      val newInput = inputReader.compile(input, sig)
      dbInput = dbInput.combine(newInput)
    }
    dbInput
  }

  def loadInputs(inputs: Map[RuleSignature, String], compiled: CompiledSouffleModule, delimiter: String = "\t"): DatabaseInput = {
    val inputReader = getSouffleInputReader("EMPTY", compiled)
    var dbInput: DatabaseInput = DatabaseInput.empty
    inputs.foreach { case (sig, content) =>
      val rows = content.split("\n")
      val newInput = inputReader.compile(rows.toIterator, sig, delimiter)
      dbInput = dbInput.combine(newInput)
    }
    dbInput
  }

  def compileSouffle(code: Source, options: SouffleOptions = SouffleOptions()): CompiledSouffleModule = {
    val ast = Parser.parse(code)
    val compiler = new SouffleToDatalogIR(options.useEditScriptsForInput)
    compiler.compile("soufflemod", ast)
  }

  def loadAnalysis(code: Source, options: SouffleOptions = SouffleOptions()): Loaded = {
    val compiled = compileSouffle(code, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, options.mode)
    Loaded(engine, feed, compiled)
  }
}
