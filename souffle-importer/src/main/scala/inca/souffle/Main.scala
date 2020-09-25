package inca.souffle

import inca.CompilerOptions
import inca.backend.ir.GP.TString
import inca.backend.ir.Printer
import inca.runtime.Query.ChangeFeed
import inca.runtime.{Database, EnginePool, Query}
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truechange.EditScript

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val file = Source.fromFile(filename).getLines.mkString("\n")
    val analysis = Parser(file)
    val compiler = new SouffleToIncaCompiler
    val (gpModule, inputs, languageMetaInfo) = compiler.compile("selfcontained", analysis)

    val psModule = inca.Compiler.compileAndLoadGPModule(gpModule, None, CompilerOptions(languageMetaInfo))
    val queryScope = new QueryScope(languageMetaInfo, Seq())

    def getMatcher(fun: String): (ChangeFeed, Query.Matcher) = {
      val querySpec = psModule.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module."))
      EnginePool.loadQuery(querySpec(), queryScope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    }

    val (feed, varPointsToMatcher) = getMatcher("VarPointsTo")
    val (_, initializedClassMatcher) = getMatcher("InitializedClass")
    val (_, assignMatcher) = getMatcher("Assign")
    val (_, instanceFieldPointsToMatcher) = getMatcher("InstanceFieldPointsTo")
    val (_, staticFieldPointsToMatcher) = getMatcher("StaticFieldPointsTo")
    val (_, reachableMatcher) = getMatcher("Reachable")
    val (_, callGraphEdgeMatcher) = getMatcher("CallGraphEdge")
    val (_, arrayIndexPointsToMatcher) = getMatcher("ArrayIndexPointsTo")
    // varpointsto_rel is varpointsto
    // varpointsto0 factors out one specific case of varpointsto
    // assign0 factors out callgraphedge and formalparam of first assign rule (avoid join of formal and actual param)
    // interproc0, interproc1, interproc2 are the different cases of CallGraphEdge

    val edit = inputs.flatMap { case (sig, input) =>
      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
      val editScript = inputCompiler.compile(input, sig)
      println(input.rule)
      println(editScript.size)
      editScript.edits
    }

    println(varPointsToMatcher.countMatches())
    println(initializedClassMatcher.countMatches())
    println(assignMatcher.countMatches())
    println(instanceFieldPointsToMatcher.countMatches())
    println(staticFieldPointsToMatcher.countMatches())
    println(reachableMatcher.countMatches())
    println(callGraphEdgeMatcher.countMatches())
    println(arrayIndexPointsToMatcher.countMatches())
  }
}