package inca.integration

import inca.Executor._
import inca.examples.Code
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsDataTest extends AnyFunSuite {

  test("Plus Example") {
    val fun = loadFunction(Code.plusRealModule)
    assert(fun.execute("main_bbf", Seq(q"Succ(Succ(Zero()))", q"Succ(Zero())"))
      == fun.result(q"Succ(Succ(Succ(Zero())))"))
    assert(fun.execute("main_bbf", Seq(q"Succ(Succ(Succ(Succ(Zero()))))", q"Succ(Succ(Zero()))"))
      == fun.result(q"Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))"))
    fun.printAllMatches()
  }

  test("Type Checker Example") {
    val fun = loadFunction(Code.typeOfModule)
    println("######################################################")
    assert(fun.execute("main_bf", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"Some(TInt())"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"Some(TFun(TInt(), TInt()))"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.result(q"None()"))
    assert(fun.execute("main_bf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"Some(TInt())"))
    assert(fun.execute("main_bf", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.result(q"None()"))
    fun.printAllMatches()
  }

  test("Type Erasure Example") {
    val fun = loadFunction(Code.eraseModule)
    assert(fun.execute("main_bf", Seq(q"TNum(1)"), deleteInput = true)
      == fun.result(q"Num(1)"))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("x"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("x"))"""))
    assert(fun.execute("main_bf", Seq(q"""TLam("x", TInt(), TVar("y"))"""), deleteInput = true)
      == fun.result(q"""Lam("x", Var("y"))"""))
    assert(fun.execute("main_bf", Seq(q"""TApp(TLam("x", TInt(), TVar("x")), TNum(1337))"""), deleteInput = true)
      == fun.result(q"""App(Lam("x", Var("x")), Num(1337))"""))
    assert(fun.execute("main_bf", Seq(q"""TApp(TNum(12), TNum(11))"""), deleteInput = true)
      == fun.result(q"App(Num(12), Num(11))"))
    fun.printAllMatches()
  }

  test("Interpreter Example") {
    val fun = loadFunction(Code.interpModule)
    assert(fun.execute("main_bf", Seq(q"Num(1)"), deleteInput = true)
      == fun.result(q"Some(VNum(1))"))
    assert(fun.execute("main_bf", Seq(q"""Lam("x", Var("x"))"""), deleteInput = true)
      == fun.result(q"""Some(VClosure("x", Var("x"), Empty()))"""))
    assert(fun.execute("main_bf", Seq(q"""Lam("x", Var("y"))"""), deleteInput = true)
      == fun.result(q"""Some(VClosure("x", Var("y"), Empty()))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("y", Lam("x", Var("y"))), Num(1))"""), deleteInput = true)
      == fun.result(q"""Some(VClosure("x", Var("y"), Bind("y", VNum(1), Empty())))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("x", Var("y")), Num(1))"""), deleteInput = true)
      == fun.result(q"""None()"""))
    assert(fun.execute("main_bf", Seq(q"""App(Lam("x", Var("x")), Num(1337))"""), deleteInput = true)
      == fun.result(q"""Some(VNum(1337))"""))
    assert(fun.execute("main_bf", Seq(q"""App(Num(12), Num(11))"""), deleteInput = true)
      == fun.result(q"None()"))
    fun.printAllMatches()
  }




//  test("Adornment with fixed adornment") {
//    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
//    val adorned = AdornProgram.transformer.transformModule(moduleGP)
//
//    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
//    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))
//    val scope = new QueryScope(NAT_lmi)
//    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//
//    def printMatches(name: String): Unit = {
//      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//      println(s"matches of $name:   ${matcher.getAllMatches}")
//    }
//
//    compiled.psystemModule.patterns.keys.foreach(printMatches)
//
//    val mainMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("main_f")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//    assert(mainMatcher.getAllMatches.size == 1)
//    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero())))))"))
//  }
//
//  test("Adornment with fixed adornment (real plus)") {
//    val moduleGP = GenerateDatalog.transformModule(AST.plusRealModule)
//    val adorned = AdornProgram.transformer.transformModule(moduleGP)
//    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
//    println(magicSet)
//    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))
//
//    val scope = new QueryScope(NAT_lmi)
//    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//
//    def printMatches(name: String): Unit = {
//      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
//    }
//
//    // first argument of main
//    val zero0 = new DataURI("Zero")
//    val succ1 = new DataURI("Succ(Zero)")
//    val succ2 = new DataURI("Succ(Succ(Zero))")
//    val succ3 = new DataURI("Succ(Succ(Succ(Zero)))")
//    feed.processEdit(Load(zero0, NamedTag("Zero"), Seq(), Seq()))
//    feed.processEdit(Load(succ1, NamedTag("Succ"), Seq(("_0", zero0)), Seq()))
//    feed.processEdit(Load(succ2, NamedTag("Succ"), Seq(("_0", succ1)), Seq()))
//    feed.processEdit(Load(succ3, NamedTag("Succ"), Seq(("_0", succ2)), Seq()))
//
//    // second argument of main
//    val zero1 = new DataURI("Zero")
//    val succ4 = new DataURI("Succ(Zero)")
//    val succ5 = new DataURI("Succ(Succ(Zero))")
//    feed.processEdit(Load(zero1, NamedTag("Zero"), Seq(), Seq()))
//    feed.processEdit(Load(succ4, NamedTag("Succ"), Seq(("_0", zero1)), Seq()))
//    feed.processEdit(Load(succ5, NamedTag("Succ"), Seq(("_0", succ4)), Seq()))
//
//    feed.insert("ext_input_main_bbf", Tuples.flatTupleOf(succ3, succ5))
//
//    compiled.psystemModule.patterns.keys.foreach(printMatches)
//    val mainMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("main_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//    assert(mainMatcher.getAllMatches.size == 1)
//    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero)))))"))
//
//
//    println()
//    println("insert new main input")
//    val succ6 = new DataURI("Succ(Succ(Succ(Zero)))")
//    feed.processEdit(Load(succ6, NamedTag("Succ"), Seq(("_0", succ3)), Seq()))
//    feed.insert("ext_input_main_bbf", Tuples.flatTupleOf(succ6, succ5))
//    compiled.psystemModule.patterns.keys.foreach(printMatches)
//    assert(mainMatcher.getAllMatches.size == 2)
//    val matches = mainMatcher.getAllMatches().asScala.map(_.get("out").toString)
//    assert(matches.count(_.startsWith("Succ(Succ(Succ(Succ(Succ(Zero)))))@")) == 1)
//    assert(matches.count(_.startsWith("Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))@")) == 1)
//
//    println()
//    println("delete old main input")
//    feed.delete("ext_input_main_bbf", Tuples.flatTupleOf(succ3, succ5))
//    compiled.psystemModule.patterns.keys.foreach(printMatches)
//    assert(mainMatcher.getAllMatches.size == 1)
//    assert(mainMatcher.getOneArbitraryMatch.get().get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Succ(Zero))))))@"))
//  }
//
//  test("Adornment with fixed adornment (real plus, no main)") {
//    val moduleGP = GenerateDatalog.transformModule(AST.module(Nat, plusFun.copy(annos = Seq(MainFunctionAnno))))
//    val adorned = AdornProgram.transformer.transformModule(moduleGP)
//    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
//    println(magicSet)
//
//    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))
//    val scope = new QueryScope(NAT_lmi)
//    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//
//    def zero: Call = Call(Name("Zero"), Nil)
//    def succ(arg: Call): Call = {
//      Call(Name("Succ"), Seq(arg))
//    }
//
//    val (arg1es, arg1uri) = deriveInput(succ(succ(succ(zero))))
//    val (arg2es, arg2uri) = deriveInput(succ(succ(zero)))
//    feed.processEditScript(EditScript(arg1es))
//    feed.processEditScript(EditScript(arg2es))
//    feed.insert("ext_input_plus_bbf", Tuples.flatTupleOf(arg1uri, arg2uri))
//
//    def printMatches(name: String): Unit = {
//      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
//    }
//
//    compiled.psystemModule.patterns.keys.foreach(printMatches)

// TODO: support easier testing
//    import scala.meta._
//    val ast = Meta.compileAndLoadScala[Diffable](q"Succ(Succ(Succ(Zero())))".syntax)()
//    val oldinput = ast
//    println(ast.toStringWithURI)
//    val edits1 = Diffable.load(ast)
//    val ast2 = Meta.compileAndLoadScala[Diffable](q"Succ(Succ(Succ(Succ(Zero()))))".syntax)()
//    val (edits2, ast2_) = ast.compareTo(ast2)
//    val newinput = ast2_
//    feed.processEditScript(edits1)
//    feed.processEditScript(edits2)
//    if (oldinput.uri != newinput.uri) {
//      feed.insert("ext_input_plus_bbf", Tuples.flatTupleOf(newinput))
//      feed.delete("ext_input_plus_bbf", Tuples.flatTupleOf(oldinput))
//    }


//    val plusMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("plus_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
//    assert(plusMatcher.getAllMatches.size == 4)
//    val resultExists = plusMatcher.getAllMatches.asScala.exists { m =>
//      m.get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero())))))")
//    }
//    assert(resultExists)
//  }
}
