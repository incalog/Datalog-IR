package inca.integration

import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.{Compiler, Options}
import inca.frontend.core.{Call, MainFunctionAnno, Name}
import inca.frontend.examples.ADT.Nat
import inca.frontend.examples.AST
import inca.frontend.examples.AST.plusFun
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.data.DataURI
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truechange.{Edit, EditScript, Load, NamedTag, SortType}

import scala.:+
import scala.collection.convert.ImplicitConversions.`collection AsScalaIterable`
import scala.collection.immutable.MultiDict

class FunctionalTests extends AnyFunSuite {


  def deriveInput(call: Call): (Seq[Edit], DataURI) = {

    def deriveStringRep(call: Call): String = {
      // we only allow calls
      val argsStringRep = call.args.map(a => deriveStringRep(a.asInstanceOf[Call]))
      call.name + argsStringRep.mkString("(", ", ", ")")
    }

    val newURI = new DataURI(deriveStringRep(call))
    val (subes, subURI) = call.args.map(a => deriveInput(a.asInstanceOf[Call])).unzip

    val kids = subURI.zipWithIndex.map { case (uri, ix) => s"_$ix" -> uri }

    (subes.flatten :+ Load(newURI, NamedTag(call.name.name), kids, Seq()), newURI)
  }

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

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"matches of $name:   ${matcher.getAllMatches}")
    }

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
      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }

    // first argument of main
    val zero0 = new DataURI("Zero")
    val succ1 = new DataURI("Succ(Zero)")
    val succ2 = new DataURI("Succ(Succ(Zero))")
    val succ3 = new DataURI("Succ(Succ(Succ(Zero)))")
    feed.processEdit(Load(zero0, NamedTag("Zero"), Seq(), Seq()))
    feed.processEdit(Load(succ1, NamedTag("Succ"), Seq(("_0", zero0)), Seq()))
    feed.processEdit(Load(succ2, NamedTag("Succ"), Seq(("_0", succ1)), Seq()))
    feed.processEdit(Load(succ3, NamedTag("Succ"), Seq(("_0", succ2)), Seq()))

    // second argument of main
    val zero1 = new DataURI("Zero")
    val succ4 = new DataURI("Succ(Zero)")
    val succ5 = new DataURI("Succ(Succ(Zero))")
    feed.processEdit(Load(zero1, NamedTag("Zero"), Seq(), Seq()))
    feed.processEdit(Load(succ4, NamedTag("Succ"), Seq(("_0", zero1)), Seq()))
    feed.processEdit(Load(succ5, NamedTag("Succ"), Seq(("_0", succ4)), Seq()))

    feed.insert("ext_input_main_bbf", Tuples.flatTupleOf(succ3, succ5))

    compiled.psystemModule.patterns.keys.foreach(printMatches)
    val mainMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("main_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    assert(mainMatcher.getAllMatches.size == 1)
    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero)))))"))


    println()
    println("insert new main input")
    val succ6 = new DataURI("Succ(Succ(Succ(Zero)))")
    feed.processEdit(Load(succ6, NamedTag("Succ"), Seq(("_0", succ3)), Seq()))
    feed.insert("ext_input_main_bbf", Tuples.flatTupleOf(succ6, succ5))
    compiled.psystemModule.patterns.keys.foreach(printMatches)
    import scala.jdk.CollectionConverters._
    assert(mainMatcher.getAllMatches.size == 2)
    val matches = mainMatcher.getAllMatches().asScala.map(_.get("out").toString)
    assert(matches.count(_.startsWith("Succ(Succ(Succ(Succ(Succ(Zero)))))@")) == 1)
    assert(matches.count(_.startsWith("Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))@")) == 1)

    println()
    println("delete old main input")
    feed.delete("ext_input_main_bbf", Tuples.flatTupleOf(succ3, succ5))
    compiled.psystemModule.patterns.keys.foreach(printMatches)
    assert(mainMatcher.getAllMatches.size == 1)
    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))@"))
  }

  test("Adornment with fixed adornment (real plus, no main)") {
    val moduleGP = GenerateDatalog.transformModule(AST.module(Nat, plusFun.addAnnotation(MainFunctionAnno)))
    val adorned = AdornProgram.transformer.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(lmi))
    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

    def zero: Call = Call(Name("Zero"), Nil)
    def succ(arg: Call): Call = {
      Call(Name("Succ"), Seq(arg))
    }

    val (arg1es, arg1uri) = deriveInput(succ(succ(succ(zero))))
    val (arg2es, arg2uri) = deriveInput(succ(succ(zero)))
    feed.processEditScript(EditScript(arg1es))
    feed.processEditScript(EditScript(arg2es))
    feed.insert("ext_input_plus_bbf", Tuples.flatTupleOf(arg1uri, arg2uri))

    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)

    val plusMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("plus_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    assert(plusMatcher.getAllMatches.size == 4)
    val resultExists = plusMatcher.getAllMatches.exists { m =>
      m.get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero())))))")
    }
    assert(resultExists)
  }
}
