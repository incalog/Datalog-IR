package inca.integration

import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.{Compiler, Options}
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

class FunctionalTests extends AnyFunSuite {

  test("Adornment with fixed adornment") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
    val trans = AdornProgram.transformer
    val adorned = trans.transformModule(moduleGP)

    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    val compiled = Compiler.compileGP(magicSet, Options(new LanguageMetaInfo()))
    val scope = new QueryScope(new LanguageMetaInfo())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    // TODO actual test
    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

  test("Adornment with fixed adornment (real plus)") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusRealModule)
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(new LanguageMetaInfo()))
    val scope = new QueryScope(new LanguageMetaInfo())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

}
