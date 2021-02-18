package inca.backend.transform.magic

import inca.backend.ir.GP
import inca.backend.transform.magic.Examples._
import inca.compiler.{Compiler, Options}
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

class AdornProgramTest extends AnyFunSuite {

  def moduleEqual(m1: GP.Module, m2: GP.Module): Boolean =
    m1.pats.size == m2.pats.size && m1.pats.forall(m2.pats.contains)

  test("Adornment of flat function") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(incModuleGP)
    assert(moduleEqual(adorned, adornedIncModuleGP))
  }

  test("Adornment of recursive function") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(factModuleGP)
    assert(moduleEqual(adorned, adornedFactModuleGP))
  }

  test("Adornment of negative call") {
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(unreachableModuleGP)
    assert(moduleEqual(adorned, adornedUnreachableModuleGP))
  }

  test("Adornment with fixed adornment") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(new LanguageMetaInfo(), optimizations = Seq()))
    val scope = new QueryScope(new LanguageMetaInfo())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

  test("Adornment with fixed adornment (real plus)") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusRealModule)
    val trans = AdornProgram.transformer

    val adorned = trans.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(new LanguageMetaInfo(), optimizations = Seq()))
    val scope = new QueryScope(new LanguageMetaInfo())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }
}
