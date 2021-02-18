package inca.integration

import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.{Compiler, Options}
import inca.frontend.core.MainFunctionAnno
import inca.frontend.examples.ADT.Nat
import inca.frontend.examples.AST
import inca.frontend.examples.AST.plusFun
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truechange.{JVMURI, Load, NamedTag, SortType}

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

//    println(compiled.ir)
//    println(compiled.psystemSource)

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    // TODO actual test
    compiled.psystemModule.patterns.keys.foreach(printMatches)

    val mainMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("main_f")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    assert(mainMatcher.getAllMatches.size == 1)
    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero())))))"))
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

  test("Adornment with fixed adornment (real plus, no main)") {
    val moduleGP = GenerateDatalog.transformModule(AST.module(Nat, plusFun.addAnnotation(MainFunctionAnno)))
    val adorned = AdornProgram.transformer.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(lmi))
    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    // TODO: load data -> edit script
//    val three: truechange.URI = ???
//    val two: truechange.URI = ???
    val zero0 = new JVMURI
    feed.processEdit(Load(zero0, NamedTag("Zero"), Seq(), Seq()))
    val zero1 = new JVMURI
    feed.processEdit(Load(zero1, NamedTag("Zero"), Seq(), Seq()))

    // TODO: insert input tuple
    feed.insert("ext_input_plus_bbf", Tuples.flatTupleOf(zero0, zero1))

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }
}
