package inca.souffle

import inca.runtime.Database
import truechange.EditScript

import scala.io.Source

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val src = Source.fromFile(filename)
    val doopText = src.getLines.mkString("\n")
    src.close()
    val analysis = Parser(doopText)
    val compiler = new SouffleToIncaCompiler
    val (gpModule, inputs, languageMetaInfo) = compiler.compile("selfcontained", analysis)
//    println(gpModule)

//    val psModule = inca.Compiler.compileAndLoadGPModule(gpModule, None, CompilerOptions(languageMetaInfo))
//    val queryScope = new QueryScope(languageMetaInfo, Seq())
//
//    def getMatcher(fun: String): (ChangeFeed, Query.Matcher) = {
//      val querySpec = psModule.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module."))
//      EnginePool.loadQuery(querySpec(), queryScope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//    }

//    val (feed, varPointsToMatcher) = getMatcher("VarPointsTo")
//    val (_, initializedClassMatcher) = getMatcher("InitializedClass")
//    val (_, assignMatcher) = getMatcher("Assign")
//    val (_, instanceFieldPointsToMatcher) = getMatcher("InstanceFieldPointsTo")
//    val (_, staticFieldPointsToMatcher) = getMatcher("StaticFieldPointsTo")
//    val (_, reachableMatcher) = getMatcher("Reachable")
//    val (_, callGraphEdgeMatcher) = getMatcher("CallGraphEdge")
//    val (_, arrayIndexPointsToMatcher) = getMatcher("ArrayIndexPointsTo")
    // varpointsto_rel is varpointsto
    // varpointsto0 factors out one specific case of varpointsto
    // assign0 factors out callgraphedge and formalparam of first assign rule (avoid join of formal and actual param)
    // interproc0, interproc1, interproc2 are the different cases of CallGraphEdge

    val database = new Database(languageMetaInfo, Seq(), null)

    val lessInputs = inputs.take(10)

    val startLoadFactFiles = System.currentTimeMillis()
    val edits = lessInputs.flatMap { case (sig, input) =>
      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
      val editScript = inputCompiler.compile(input, sig)
      editScript.edits
    }
    val endLoadFactFiles = System.currentTimeMillis()
    println(s"Load fact files: ${endLoadFactFiles-startLoadFactFiles}ms")

    val editScript = EditScript(edits)
    println(editScript.size)

    val start = System.currentTimeMillis()
    database.processEditScript(editScript)
    val end = System.currentTimeMillis()
    //    println("Used memory: " + MeasurementUtils.usedMemoryInMBytes())
    val duration = end - start
    println(s"Time to fill database: ${duration}ms")

    //    println(varPointsToMatcher.countMatches())
//    println(initializedClassMatcher.countMatches())
//    println(assignMatcher.countMatches())
//    println(instanceFieldPointsToMatcher.countMatches())
//    println(staticFieldPointsToMatcher.countMatches())
//    println(reachableMatcher.countMatches())
//    println(callGraphEdgeMatcher.countMatches())
//    println(arrayIndexPointsToMatcher.countMatches())
  }
}