package inca.integration

import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.{Compiler, Options}
import inca.frontend.examples.AST
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truechange.SortType

import scala.collection.immutable.MultiDict

class FunctionalTests extends AnyFunSuite {

  private val lmi = new LanguageMetaInfo(
    MultiDict(
      SortType("Zero") -> SortType("Nat"),
      SortType("Succ") -> SortType("Nat")),
    Map(
      ("Succ", "_0") -> SortType("Nat")
    ),
    Map()
  )

  test("Adornment with fixed adornment") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
    val adorned = AdornProgram.transformer.transformModule(moduleGP)

    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    val compiled = Compiler.compileGP(magicSet, Options(lmi))
    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    println(compiled.ir)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    // TODO actual test
    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

  test("Adornment with fixed adornment (real plus)") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusRealModule)
    val adorned = AdornProgram.transformer.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(lmi))
    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

}
