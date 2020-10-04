package inca.souffle



import inca.CompilerOptions
import inca.runtime.EnginePool
import inca.runtime.Query.Matcher
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.{DRedReteBackendFactory, TimelyReteBackendFactory}
import truechange.{Edit, EditScript, Load, NamedTag, Unload}

import scala.io.Source
import scala.util.Random

object Main {
  def main(args: Array[String]): Unit = {
    val filename = "souffle-importer/self-contained.dl"
    val src = Source.fromFile(filename)
    val doopText = src.getLines.mkString("\n")
    src.close()
    val analysis = Parser(doopText)
    val compiler = new SouffleToIncaCompiler
    val (gpModule, inputs, printSizes, languageMetaInfo) = compiler.compile("selfcontained", analysis)

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

    val heapAllocationLoads = editScript.edits.collect { case l: Load => l}.filter { l => l.tag == NamedTag("_AssignHeapAllocation") }
    val invalidPrefixes = Seq("<sun", "<java", "<com")
    val impactfullLoads = heapAllocationLoads.filter { l =>
      val first = l.lits.head._2.asInstanceOf[String]
      !invalidPrefixes.exists(first.startsWith)
    }

    val varPointsToMatcher = matchers.find(_.getPatternName.endsWith("VarPointsTo")).get
    val rng = new Random(1563540296429L)
    val indices = impactfullLoads.indices
    val shuffledIndices = rng.shuffle(indices.toList)


    def measureEdit(edit: Edit): Unit = {
      val startReinsert = System.currentTimeMillis()
      database.processEdit(edit)
      val timeReinsert = System.currentTimeMillis() - startReinsert
      println(s"$timeReinsert\t${varPointsToMatcher.countMatches()}")
    }

    shuffledIndices.take(2000).foreach { index =>
      val load = impactfullLoads(index)
      val unload = Unload(load.node, load.tag, load.kids, load.lits)
      measureEdit(unload)
      measureEdit(load)
    }
  }

}