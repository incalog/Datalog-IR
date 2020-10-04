package inca.souffle


import inca.CompilerOptions
import inca.backend.transform.PropagateUnbounded
import inca.runtime.EnginePool
import inca.runtime.Query.Matcher
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.{DRedReteBackendFactory, TimelyReteBackendFactory}
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
    val (gpModule, inputs, printSizes, languageMetaInfo) = compiler.compile("selfcontained", analysis)
    println(gpModule)

    val psModule = inca.Compiler.compileAndLoadGPModule(gpModule, None, CompilerOptions(languageMetaInfo))
    val queryScope = new QueryScope(languageMetaInfo, Seq())

    def getMatcher(fun: String): Matcher = {
      val querySpec = psModule.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module."))
      EnginePool.loadQuery(querySpec(), queryScope, DRedReteBackendFactory.INSTANCE)
    }

    val (engine, database) = EnginePool.loadEngineAndDatabase(queryScope, DRedReteBackendFactory.INSTANCE)

    val startLoadFactFiles = System.currentTimeMillis()
    val edits = inputs.flatMap { case (sig, input) =>
      val inputCompiler = new SouffleInputToEditscript("souffle-importer/minijavac")
      val editScript = inputCompiler.compile(input, sig)
      editScript.edits
    }
    val endLoadFactFiles = System.currentTimeMillis()
    println(s"Load fact files: ${endLoadFactFiles-startLoadFactFiles}ms")

    val editScript = EditScript(edits)
    println(editScript.size)

    val matchers = printSizes.map(ps => getMatcher(ps.name))

    val startQuery = System.currentTimeMillis()
    var loadingTime: Long = 0
    engine.delayUpdatePropagation {() =>
      val startLoadDB = System.currentTimeMillis()
      database.processEditScript(editScript)
      val endLoadDB = System.currentTimeMillis()
      loadingTime = endLoadDB - startLoadDB
    }
    val endQuery = System.currentTimeMillis()
    println(s"Time to fill database: ${loadingTime}ms")
    println("Used memory: " + MeasurementUtils.usedMemoryInMBytes())
    println(s"Time to process query: ${endQuery - startQuery}ms")

    matchers.foreach { m =>
      println(s"${m.getPatternName}: ${m.countMatches()}")
    }
  }
}