package inca.integration

import inca.backend.transform.magic.{AdornProgram, MagicSetTransformation}
import inca.compiler.{Compiler, Options}
import inca.frontend.core.{Call, MainFunctionAnno, Name}
import inca.frontend.examples.ADT.{NAT_lmi, Nat}
import inca.frontend.examples.AST.plusFun
import inca.frontend.examples.{AST, Code}
import inca.frontend.lowering.GenerateDatalog
import inca.runtime.EnginePool
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.runtime.data.DataURI
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truechange._

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

  test("Adornment with fixed adornment") {
    val moduleGP = GenerateDatalog.transformModule(AST.plusModule)
    val adorned = AdornProgram.transformer.transformModule(moduleGP)

    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))
    val scope = new QueryScope(NAT_lmi)
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
    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))

    val scope = new QueryScope(NAT_lmi)
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
    val moduleGP = GenerateDatalog.transformModule(AST.module(Nat, plusFun.copy(annos = Seq(MainFunctionAnno))))
    val adorned = AdornProgram.transformer.transformModule(moduleGP)
    val magicSet = MagicSetTransformation.transformer.transformModule(adorned)
    println(magicSet)

    val compiled = Compiler.compileGP(magicSet, Options(NAT_lmi))
    val scope = new QueryScope(NAT_lmi)
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


    val plusMatcher = EnginePool.loadQuery(compiled.psystemModule.patterns("plus_bbf")(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    assert(plusMatcher.getAllMatches.size == 4)
    val resultExists = plusMatcher.getAllMatches.exists { m =>
      m.get("out").toString.startsWith("Succ(Succ(Succ(Succ(Succ(Zero())))))")
    }
    assert(resultExists)
  }

  def executeFunction(code: String, lmi: LanguageMetaInfo = new LanguageMetaInfo()): Unit = {
    val options = Options(lmi, transformations = Options.defaultTransformations)
    val compiled = Compiler.compileFun(code, options)

    val scope = new QueryScope(lmi)
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)


    def printMatches(name: String): Unit = {
      val matcher = EnginePool.loadQuery(compiled.psystemModule.patterns(name)(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
      println(s"${matcher.getAllMatches.size()} matches of $name:   ${matcher.getAllMatches}")
    }

    compiled.psystemModule.patterns.keys.foreach(printMatches)
  }

  test("Factorial Example") {
    executeFunction(Code.factModule)
  }

  test("Fibonacci Example") {
    executeFunction(Code.fibModule)
  }

  test("TypeChecker Example") {
    val lmi: LanguageMetaInfo = new LanguageMetaInfo(
      MultiDict(
        SortType("TInt") -> SortType("Type"),
        SortType("TFun") -> SortType("Type"),
        SortType("None") -> SortType("MaybeType"),
        SortType("Some") -> SortType("MaybeType"),
        SortType("Num") -> SortType("Exp"),
        SortType("Lam") -> SortType("Exp"),
        SortType("App") -> SortType("Exp"),
        SortType("Var") -> SortType("Exp"),
        SortType("Empty") -> SortType("Ctx"),
        SortType("Bind") -> SortType("Ctx")
      ),
      Map(
        ("TFun", "_0") -> SortType("Type"),
        ("TFun", "_1") -> SortType("Type"),
        ("Some", "_0") -> SortType("Type"),
        ("Lam", "_1") -> SortType("Type"),
        ("Lam", "_2") -> SortType("Exp"),
        ("App", "_0") -> SortType("Exp"),
        ("App", "_1") -> SortType("Exp"),
        ("Bind", "_1") -> SortType("Exp"),
        ("Bind", "_2") -> SortType("Ctx")
      ),
      Map(
        ("Num", "_0") -> JavaLitType(classOf[Int]),
        ("Lam", "_0") -> JavaLitType(classOf[String]),
        ("Var", "_0") -> JavaLitType(classOf[String]),
        ("Bind", "_0") -> JavaLitType(classOf[String])
      )
    )

    val typeOfCode =
      """module Typing
        |data Type = TInt() | TFun(Type, Type)
        |data MaybeType = None() | Some(Type)
        |data Exp = Num(`Int`) | Lam(`String`, Type, Exp) | App(Exp, Exp) | Var(`String`)
        |data Ctx = Empty() | Bind(`String`, Type, Ctx)
        |
        |@main def main(): MaybeType = let exp = Num(`1`) in typeOf(Empty(), exp)
        |
        |def typeOf(ctx: Ctx, exp: Exp): MaybeType = exp match {
        |  case Num(v) => Some(TInt())
        |  case Lam(n, ty, b) =>
        |    let extCtx = Bind(n, ty, ctx) in
        |      let mbty2 = typeOf(extCtx, b) in
        |        mbty2 match {
        |          case Some(ty2) => Some(TFun(ty, ty2))
        |          case None() => None()
        |        }
        |  case App(fun, arg) =>
        |    let mbfunty = typeOf(ctx, fun) in
        |      mbfunty match {
        |        case Some(funty) =>
        |          funty match {
        |            case TInt() => None()
        |            case TFun(ty1, ty2) =>
        |              let mbargty = typeOf(ctx, arg) in
        |                mbargty match {
        |                  case Some(argty) =>
        |                    if (ty1 == argty) Some(ty2)
        |                     else None()
        |                  case None() => None()
        |                }
        |          }
        |        case None() => None()
        |      }
        |  case Var(n) => lookup(ctx, n)
        |}
        |
        |def lookup(ctx: Ctx, n: `String`): MaybeType = ctx match {
        |  case Empty() => None()
        |  case Bind(n1, ty, rest) =>
        |    if (n1 == n) Some(ty)
        |    else lookup(rest, n)
        |}
        |""".stripMargin

    executeFunction(typeOfCode, lmi)
  }

  test("TypeErasure Example") {
    val lmi: LanguageMetaInfo = new LanguageMetaInfo(
      MultiDict(
        SortType("TInt") -> SortType("Type"),
        SortType("TFun") -> SortType("Type"),
        SortType("TNum") -> SortType("TExp"),
        SortType("TLam") -> SortType("TExp"),
        SortType("TApp") -> SortType("TExp"),
        SortType("TVar") -> SortType("TExp"),
        SortType("Num") -> SortType("Exp"),
        SortType("Lam") -> SortType("Exp"),
        SortType("App") -> SortType("Exp"),
        SortType("Var") -> SortType("Exp"),
      ),
      Map(
        ("TFun", "_0") -> SortType("Type"),
        ("TFun", "_1") -> SortType("Type"),
        ("TLam", "_1") -> SortType("Type"),
        ("TLam", "_2") -> SortType("TExp"),
        ("TApp", "_0") -> SortType("TExp"),
        ("TApp", "_1") -> SortType("TExp"),
        ("Lam", "_1") -> SortType("Exp"),
        ("App", "_0") -> SortType("Exp"),
        ("App", "_1") -> SortType("Exp"),
      ),
      Map(
        ("TNum", "_0") -> JavaLitType(classOf[Int]),
        ("TLam", "_0") -> JavaLitType(classOf[String]),
        ("TVar", "_0") -> JavaLitType(classOf[String]),
        ("Num", "_0") -> JavaLitType(classOf[Int]),
        ("Lam", "_0") -> JavaLitType(classOf[String]),
        ("Var", "_0") -> JavaLitType(classOf[String]),
      )
    )

    val typeOfCode =
      """module TypeErasure
        |data Type = TInt() | TFun(Type, Type)
        |data TExp = TNum(`Int`) | TLam(`String`, Type, TExp) | TApp(TExp, TExp) | TVar(`String`)
        |data Exp = Num(`Int`) | Lam(`String`, Exp) | App(Exp, Exp) | Var(`String`)
        |
        |@main def main(): Exp = let exp = TApp(TLam(`"x"`, TInt(), TVar(`"x"`)), TNum(`1`)) in erase(exp)
        |
        |def erase(texp: TExp): Exp = texp match {
        |  case TNum(v) => Num(v)
        |  case TLam(n, ty, b) =>
        |    let eb = erase(b) in
        |      Lam(n, eb)
        |  case TApp(fun, arg) =>
        |    let efun = erase(fun) in
        |      let earg = erase(arg) in
        |        App(efun, earg)
        |  case TVar(n) => Var(n)
        |}
        |""".stripMargin

    executeFunction(typeOfCode, lmi)
  }

  test("Interpreter Example") {
    val lmi: LanguageMetaInfo = new LanguageMetaInfo(
      MultiDict(
        SortType("Num") -> SortType("Exp"),
        SortType("Lam") -> SortType("Exp"),
        SortType("App") -> SortType("Exp"),
        SortType("Var") -> SortType("Exp"),
        SortType("VNum") -> SortType("Val"),
        SortType("VClosure") -> SortType("Val"),
        SortType("None") -> SortType("MaybeVal"),
        SortType("Some") -> SortType("MaybeVal"),
        SortType("Empty") -> SortType("Env"),
        SortType("Bind") -> SortType("Env")
      ),
      Map(
        ("Lam", "_1") -> SortType("Exp"),
        ("App", "_0") -> SortType("Exp"),
        ("App", "_1") -> SortType("Exp"),
        ("VClosure", "_1") -> SortType("Exp"),
        ("VClosure", "_2") -> SortType("Env"),
        ("Some", "_0") -> SortType("Val"),
        ("Bind", "_1") -> SortType("Val"),
        ("Bind", "_2") -> SortType("Env")
      ),
      Map(
        ("VClosure", "_0") -> JavaLitType(classOf[String]),
        ("VNum", "_0") -> JavaLitType(classOf[Int]),
        ("Num", "_0") -> JavaLitType(classOf[Int]),
        ("Lam", "_0") -> JavaLitType(classOf[String]),
        ("Var", "_0") -> JavaLitType(classOf[String]),
        ("Bind", "_0") -> JavaLitType(classOf[String]),
      )
    )

    val typeOfCode =
      """module Interpreter
        |data Exp = Num(`Int`) | Lam(`String`, Exp) | App(Exp, Exp) | Var(`String`)
        |data Env = Empty() | Bind(`String`, Val, Env)
        |data Val = VNum(`Int`) | VClosure(`String`, Exp, Env)
        |data MaybeVal = None() | Some(Val)
        |
        |@main def main(): MaybeVal = let exp = App(Lam(`"x"`, Var(`"x"`)), Num(`1`)) in interp(Empty(), exp)
        |
        |def interp(env: Env, exp: Exp): MaybeVal = exp match {
        |  case Num(v) => Some(VNum(v))
        |  case Lam(n, b) => Some(VClosure(n, b, env))
        |  case App(fun, arg) =>
        |    let mbfunv = interp(env, fun) in
        |      mbfunv match {
        |        case Some(funv) =>
        |          funv match {
        |            case VClosure(param, body, fenv) =>
        |              let mbargv = interp(env, arg) in
        |                mbargv match {
        |                  case Some(argv) =>
        |                    let extEnv = Bind(param, argv, fenv) in
        |                      interp(extEnv, body)
        |                  case None() => None()
        |                }
        |            case VNum(v) => None()
        |          }
        |        case None() => None()
        |      }
        |  case Var(n) => lookup(env, n)
        |}
        |
        |def lookup(env: Env, n: `String`): MaybeVal = env match {
        |  case Empty() => None()
        |  case Bind(n1, v, rest) =>
        |    if (n1 == n) Some(v)
        |    else lookup(rest, n)
        |}
        |""".stripMargin

    executeFunction(typeOfCode, lmi)
  }
}
