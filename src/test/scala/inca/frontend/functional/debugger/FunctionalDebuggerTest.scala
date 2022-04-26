package inca.frontend.functional.debugger

import inca.compiler.Compiler
import inca.examples.functional.ADT
import inca.examples.functional.Code
import inca.frontend.functional.compiler.CompiledFunctionalModule
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.core
import inca.frontend.functional.core.Expression
import inca.frontend.functional.core.Pattern
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.runtime.EnginePool
import meta.quasiquotes._
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion
import org.scalatest.BeforeAndAfterEach
import truechange.EditScript

class FunctionalDebuggerTest extends AnyFunSuite with BeforeAndAfterEach {

  override def afterEach(): Unit = {
    EnginePool.disposeAllEngines()
  }

  def initDebugger(module: CompiledFunctionalModule): FunctionalDebugger = {
    val debugger = new FunctionalDebugger(module)
    setupDatabaseRuntime(debugger, module.dataModel)
    debugger
  }

  def compile(code: String): CompiledFunctionalModule =
    Compiler.compileFunctional(code, FunctionalOptions().withOptimizations(Seq()))

  def setupDatabaseRuntime(
      debugger: FunctionalDebugger,
      dataModel: DataModel,
      es: EditScript = EditScript(Seq())
    ): Unit = {
    val scope = new QueryScope(dataModel)
    val (_engine, _database) =
      EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    _engine.delayUpdatePropagation(() => {
      _database.processEditScript(es)
    })
    debugger.setDatabaseRuntime(_engine, _database)
  }

  def assertControlTraceSize(
      prog: String,
      main: String,
      args: meta.Term*
    )(
      expected: Int
    ): Assertion = {
    val compiledExample = compile(prog)
    val debugger = initDebugger(compiledExample)
    debugger.entry(main, args: _*)
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertResult(expected)(debugger.controlTraceFrontend.size)
  }

  def createBreakpointOfExpression(
      f: String,
      exp: Expression,
      occurrence: Int = 0
    ): FunctionalDebugger => FunctionalBreakpoint = debugger => {
    FunctionalBreakpoint.forExpression(debugger.compiled.fun, f, exp, occurrence)
  }

  def createBreakpointOfPattern(
      f: String,
      p: Pattern,
      occurrence: Int = 0
    ): FunctionalDebugger => FunctionalBreakpoint = debugger => {
    FunctionalBreakpoint.forPattern(debugger.compiled.fun, f, p, occurrence)
  }

  def createBreakpointOfBinding(
      f: String,
      name: String,
      occurrence: Int = 0
    ): FunctionalDebugger => FunctionalBreakpoint = debugger => {
    FunctionalBreakpoint.forBinding(debugger.compiled.fun, f, name, occurrence)
  }

  def assertBreakpoints(
      prog: String,
      bps: Seq[FunctionalDebugger => FunctionalBreakpoint],
      main: String,
      args: meta.Term*
    )(
      expected: Int
    ): Assertion = {
    val compiledExample = compile(prog)
    val debugger = initDebugger(compiledExample)
    debugger.entry(main, args: _*)
    bps.foreach { bp =>
      debugger.addBreakpoint(bp(debugger))
    }
    assert(!debugger.isFinished)
    (0 until expected).foreach { _ =>
      debugger.resume()
      println(debugger.currentDebuggerInfo())
      assert(!debugger.isFinished)
    }
    debugger.resume()
    assert(debugger.isFinished)
  }

  test("nested let") {
    assertControlTraceSize(Code.varExample, "main")(6)
  }

  val tupleLetProg: String =
    s"""module M
      |@main def main(): Int =
      |  let (x, y) = (1 + 2, 2 + 3) in
      |    x + y
      |""".stripMargin
  test("multiple names let") {
    assertControlTraceSize(tupleLetProg, "main")(7)
  }

  test("nested let 2") {
    val code =
      s"""module M
        |@main def main(): Int =
        |  let x = 1 + 2 in
        |    let y = 2 + 3 in
        |      x + y
        |""".stripMargin
    assertControlTraceSize(code, "main")(7)
  }

  test("simple function call") {
    assertControlTraceSize(Code.incModule, "main")(6)
  }

  test("if example") {
    assertControlTraceSize(Code.ifExample, "main")(4)
  }

  test("if example 2") {
    assertControlTraceSize(Code.ifExample2, "main")(8)
  }

  def ifControlJump(b1: Boolean, b2: Boolean): String =
    s"""module M
      |@main def main(): Int =
      |  if ($b1 == true)
      |    if ($b2 == true)
      |      0 + 0
      |    else
      |      1 + 0
      |  else
      |    if ($b2 == true)
      |      2 + 0
      |    else
      |      3 + 0
      |""".stripMargin

  test("if control jumping") {
    for {
      b1 <- Seq(true, false)
      b2 <- Seq(true, false)
    } {
      assertControlTraceSize(ifControlJump(b1, b2), "main")(5)
    }
  }

  test("fib example") {
    assertControlTraceSize(Code.fibModule, "main", q"3")(28)
  }

  val constructorProg: String = Code.module(
    ADT.Nat_code,
    """@main def main(): Nat = Succ(Succ(Zero()))
      |""".stripMargin
  )
  test("Constructor calls example") {
    assertControlTraceSize(constructorProg, "main")(5)
  }

  val matchProg: String =
    s"""module M
      |data Exp = Var(String) | Num(Int) | Add(Exp, Exp) | Let(String, Exp, Exp)
      |@main def main(exp: Exp): Int = exp match {
      |  case Var(x) => 1
      |  case Num(i) => 2
      |  case Add(l, r) => 3
      |  case Let(n, bound, body) => 4
      |}
      |""".stripMargin

  test("pattern matching multiple constructors") {
    assertControlTraceSize(matchProg, "main", q"""Var("x")""")(3)
    assertControlTraceSize(matchProg, "main", q"""Num(1)""")(4)
    assertControlTraceSize(matchProg, "main", q"""Add(Var("y"), Num(2))""")(5)
    assertControlTraceSize(matchProg, "main", q"""Let("x", Num(3), Add(Var("x"), Num(2)))""")(6)
  }

  val nestedMatchProg: String =
    s"""module M
      |data Exp = Var(String) | Num(Int) | Add(Exp, Exp) | Let(String, Exp, Exp)
      |@main def main(exp1: Exp, exp2: Exp): Boolean = exp1 match {
      |  case Var(x1) => exp2 match {
      |    case Var(x2) => true
      |    case Num(i2) => false
      |    case Add(l2, r2) => false
      |    case Let(n2, bound2, body2) => false
      |  }
      |  case Num(i1) => exp2 match {
      |    case Var(x2) => false
      |    case Num(i2) => true
      |    case Add(l2, r2) => false
      |    case Let(n2, bound2, body2) => false
      |  }
      |  case Add(l1, r1) => exp2 match {
      |    case Var(x2) => false
      |    case Num(i2) => false
      |    case Add(l2, r2) => true
      |    case Let(n2, bound2, body2) => false
      |    }
      |  case Let(n1, bound1, body1) => exp2 match {
      |    case Var(x2) => false
      |    case Num(i2) => false
      |    case Add(l2, r2) => false
      |    case Let(n2, bound2, body2) => true
      |  }
      |}
      |""".stripMargin

  test("nested pattern matching") {
    val (v, n, a, l) =
      (q"""Var("x")""", q"Num(1)", q"Add(Num(1), Num(2))", q"""Let("x", Num(1), Num(2))""")

    assertControlTraceSize(nestedMatchProg, "main", v, v)(4)
    assertControlTraceSize(nestedMatchProg, "main", v, n)(5)
    assertControlTraceSize(nestedMatchProg, "main", v, a)(6)
    assertControlTraceSize(nestedMatchProg, "main", v, l)(7)
    assertControlTraceSize(nestedMatchProg, "main", n, v)(5)
    assertControlTraceSize(nestedMatchProg, "main", n, n)(6)
    assertControlTraceSize(nestedMatchProg, "main", n, a)(7)
    assertControlTraceSize(nestedMatchProg, "main", n, l)(8)
    assertControlTraceSize(nestedMatchProg, "main", a, v)(6)
    assertControlTraceSize(nestedMatchProg, "main", a, n)(7)
    assertControlTraceSize(nestedMatchProg, "main", a, a)(8)
    assertControlTraceSize(nestedMatchProg, "main", a, l)(9)
    assertControlTraceSize(nestedMatchProg, "main", l, v)(7)
    assertControlTraceSize(nestedMatchProg, "main", l, n)(8)
    assertControlTraceSize(nestedMatchProg, "main", l, a)(9)
    assertControlTraceSize(nestedMatchProg, "main", l, l)(10)
  }

  val matchInIfProg: String =
    s"""module Mod
      |data Exp = Var(String) | Num(Int) | Add(Exp, Exp)
      |@main def main(flag: Boolean, exp: Exp): Boolean =
      |  if (flag == true) {
      |    exp match {
      |      case Var(x) => true
      |      case Num(x) => false
      |      case Add(x, y) => true
      |    }
      |  } else {
      |    exp match {
      |      case Var(x) => false
      |      case Num(x) => true
      |      case Add(x, y) => true
      |    }
      |  }
      |""".stripMargin
  test("pattern match in if expression") {
    assertControlTraceSize(matchInIfProg, "main", q"true", q"""Var("x")""")(4)
    assertControlTraceSize(matchInIfProg, "main", q"true", q"Num(1)")(5)
    assertControlTraceSize(matchInIfProg, "main", q"false", q"Num(1)")(5)
    assertControlTraceSize(matchInIfProg, "main", q"false", q"""Var("x")""")(4)
  }

  val ifInMatchProg: String =
    s"""module Mod
      |data Exp = Var(String) | Num(Int) | Add(Exp, Exp)
      |@main def main(flag: Boolean, exp: Exp): Boolean = exp match {
      |  case Var(x) => if (flag == true) true else false
      |  case Num(x) => if (flag == true) false else true
      |  case Add(x, y) => true
      |}
      |""".stripMargin
  test("if expression in pattern match") {
    assertControlTraceSize(ifInMatchProg, "main", q"true", q"""Var("x")""")(4)
    assertControlTraceSize(ifInMatchProg, "main", q"true", q"Num(1)")(5)
    assertControlTraceSize(ifInMatchProg, "main", q"false", q"Num(1)")(5)
    assertControlTraceSize(ifInMatchProg, "main", q"false", q"""Var("x")""")(4)
  }

  test("plus example extra") {
    assertControlTraceSize(
      Code.plusRealModuleExtra,
      "main",
      q"Succ(Succ(Zero()))",
      q"Succ(Zero())"
    )(21)
  }

  val tupleProg: String =
    s"""module M
      |@main def main(): (Int, Boolean) = (1 + 1, true && false)
      |""".stripMargin
  test("tuple example") {
    assertControlTraceSize(tupleProg, "main")(4)
  }

  val setProg: String =
    s"""module M
      |@main def main(): Set[Int] = {1 + 1, 2 + 1, 3 + 1}
      |""".stripMargin
  test("set example") {
    assertControlTraceSize(setProg, "main")(5)
  }

  val setCompProg: String =
    s"""module M
      |@main def main: Set[Int] = { (x+1) | x in intSet()}
      |def intSet(): Set[Int] = {1, 2, 3, 4}
      |""".stripMargin
  test("set comprehension") {
    assertControlTraceSize(setCompProg, "main")(8)
  }

  val nestedBinaryProg: String =
    s"""module M
      |@main def main(): Int = (1 + 4) + (3 + 4)
      |""".stripMargin
  test("nested binary") {
    assertControlTraceSize(nestedBinaryProg, "main")(5)
  }

  val deeperNestedBinaryProg: String =
    s"""module M
      |@main def main(): Int = ((1 + 5) + 4) + (3 + 4)
      |""".stripMargin
  test("deeper nested binary") {
    assertControlTraceSize(deeperNestedBinaryProg, "main")(6)
  }

  // TODO fix we currently do not consider the set to fold over
  ignore("fold example") {
    val code: String =
      s"""module M
        |def add(x: Int, y: Int): Int = x + y
        |@main def main(): Int = fold(0, add, {1 + 1, 2 + 3, 3 + 4})
        |""".stripMargin
    assertControlTraceSize(code, "main")(6)
  }

  // TODO lambda cannot be compiled
  ignore("lambda example") {
    val code: String =
      s"""module M
        |@main def main(): Int = ((x: Int) => x + 1)(4)
        |""".stripMargin
    assertControlTraceSize(code, "main")(6)
  }

  val twoFunctionCallArgs: String = Code.module(
    ADT.Nat_code,
    s"""def plus(m: Nat, n: Nat): Nat = m match {
      |  case Zero() => n
      |  case Succ(pred) => Succ(plus(pred, n))
      |}
      |""".stripMargin,
    s"""@main def main(x: Nat, y: Nat): Nat =
      |  plus(Succ(Zero()), plus(x, y))
      |""".stripMargin
  )
  test("function with two call arguments") {
    assertControlTraceSize(twoFunctionCallArgs, "main", q"Succ(Succ(Zero()))", q"Succ(Zero())")(30)
  }

  // Breakpoint tests

  test("test main function entry breakpoint") {
    assertBreakpoints(
      Code.fibModule,
      Seq(_ => FunctionalBreakpoint(FunctionEntry("main"))),
      "main",
      q"3"
    )(0)
  }
  test("test function entry breakpoint") {
    assertBreakpoints(Code.incModule, Seq(_ => FunctionalBreakpoint(FunctionEntry("inc"))), "main")(
      1
    )
  }
  test("test function entry breakpoint of recursive function") {
    assertBreakpoints(
      Code.fibModule,
      Seq(_ => FunctionalBreakpoint(FunctionEntry("fib"))),
      "main",
      q"3"
    )(4)
  }

  test("test function exit breakpoint") {
    assertBreakpoints(
      Code.fibModule,
      Seq(_ => FunctionalBreakpoint(FunctionExit("main"))),
      "main",
      q"3"
    )(1)
  }
  test("test function exit breakpoint of recursive function") {
    assertBreakpoints(
      Code.fibModule,
      Seq(_ => FunctionalBreakpoint(FunctionExit("fib"))),
      "main",
      q"3"
    )(4)
  }

  test("test function call argument breakpoint ") {
    val expression = core.BaseApplyInfix(core.Var("n"), "-", core.BaseLit(q"1", core.TScalaInt))
    val bp = createBreakpointOfExpression("fib", expression)
    assertBreakpoints(Code.fibModule, Seq(bp), "main", q"3")(2)
  }

  test("test if condition breakpoint") {
    val expression = core.BaseApplyInfix(core.Var("n"), "==", core.BaseLit(q"0", core.TScalaInt))
    val bp = createBreakpointOfExpression("fib", expression)
    assertBreakpoints(Code.fibModule, Seq(bp), "main", q"3")(4)
  }

  val multipleIfsWithSameCond: String =
    s"""module M
      |@main def main(n: Int): Int =
      |  let x = (if (n == 0) 1 else 2) in
      |    let y = (if (n == 0) 2 else 1) in
      |      x + y
      |""".stripMargin
  test("test if condition breakpoint where condition is occuring twice in program 1") {
    val expression = core.BaseApplyInfix(core.Var("n"), "==", core.BaseLit(q"0", core.TScalaInt))
    val bp = createBreakpointOfExpression("main", expression)
    assertBreakpoints(multipleIfsWithSameCond, Seq(bp), "main", q"3")(1)
  }

  test("test if condition breakpoint where condition is occuring twice in program 2") {
    val expression = core.BaseApplyInfix(core.Var("n"), "==", core.BaseLit(q"0", core.TScalaInt))
    val bp = createBreakpointOfExpression("main", expression, 1)
    assertBreakpoints(multipleIfsWithSameCond, Seq(bp), "main", q"3")(1)
  }

  // test pattern
  test("test pattern breakpoint") {
    val pattern = core.ConstructorPattern(core.Name("Succ"), Seq(core.Name("pred")))
    val bp = createBreakpointOfPattern("plus", pattern)
    assertBreakpoints(Code.plusRealModule, Seq(bp), "main", q"Succ(Succ(Zero()))", q"Succ(Zero())")(
      2
    )
  }

  test("test pattern breakpoint occuring multiple times 1") {
    val pattern = core.ConstructorPattern(core.Name("Var"), Seq(core.Name("x2")))
    val bp = createBreakpointOfPattern("main", pattern)
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Add(Var("x"), Var("y"))""", q"Num(1)")(
      0
    )
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Var("x")""", q"Num(1)")(1)
  }
  test("test pattern breakpoint occuring multiple times 2") {
    val pattern = core.ConstructorPattern(core.Name("Var"), Seq(core.Name("x2")))
    val bp = createBreakpointOfPattern("main", pattern, 1)
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Add(Var("x"), Var("y"))""", q"Num(1)")(
      0
    )
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Num(2)""", q"Num(1)")(1)
  }
  test("test pattern breakpoint occuring multiple times 3") {
    val pattern = core.ConstructorPattern(core.Name("Var"), Seq(core.Name("x2")))
    val bp = createBreakpointOfPattern("main", pattern, 2)
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Num(1)""", q"Num(1)")(0)
    assertBreakpoints(nestedMatchProg, Seq(bp), "main", q"""Add(Var("x"), Var("y"))""", q"Num(1)")(
      1
    )
  }

  // test binding
  test("test binding breakpoint") {
    val bp = createBreakpointOfBinding("main", "x")
    assertBreakpoints(Code.varExample, Seq(bp), "main")(1)
  }

  test("test binding breakpoint in multiple let") {
    val bp1 = createBreakpointOfBinding("main", "y")
    val bp2 = createBreakpointOfBinding("main", "x")
    assertBreakpoints(tupleLetProg, Seq(bp1, bp2), "main")(2)
  }

  // test constructor call breakpoint
  test("constructor call breakpoint") {
    val expression = core.Call(core.Var("Succ"), Seq(core.Call(core.Var("Zero"), Seq())))
    val bp = createBreakpointOfExpression("main", expression)
    assertBreakpoints(constructorProg, Seq(bp), "main")(1)
  }

  test("breakpoint in tuple") {
    val expression = core.BaseApplyInfix(
      core.BaseLit(q"1", core.TScalaInt),
      "+",
      core.BaseLit(q"1", core.TScalaInt)
    )
    val bp = createBreakpointOfExpression("main", expression)
    assertBreakpoints(tupleProg, Seq(bp), "main")(1)
  }

  test("breakpoint in set") {
    val expression = core.BaseApplyInfix(
      core.BaseLit(q"1", core.TScalaInt),
      "+",
      core.BaseLit(q"1", core.TScalaInt)
    )
    val bp = createBreakpointOfExpression("main", expression)
    assertBreakpoints(setProg, Seq(bp), "main")(1)
  }

  test("breakpoint in nested binary") {
    val expression = core.BaseApplyInfix(
      core.BaseLit(q"1", core.TScalaInt),
      "+",
      core.BaseLit(q"4", core.TScalaInt)
    )
    val bp = createBreakpointOfExpression("main", expression)
    assertBreakpoints(nestedBinaryProg, Seq(bp), "main")(1)
  }

  // step out tests
  test("step out of function call") {
    val compiledExample = compile(Code.plusRealModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOut()
    assert(!debugger.isFinished)
    debugger.stepOut()
    assert(debugger.isFinished)
  }

  test("step out of recursive function call") {
    val compiledExample = compile(Code.plusRealModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepInto()
    debugger.stepOut()
    assert(!debugger.isFinished)
    debugger.stepOut()
    assert(!debugger.isFinished)
    debugger.stepOut()
    assert(debugger.isFinished)
  }

  // step over tests
  test("step over function entry") {
    val compiledExample = compile(Code.plusRealModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepOver()
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step over function entry of recursive function") {
    val compiledExample = compile(Code.plusRealModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step over function entry of recursive call of function") {
    val compiledExample = compile(Code.plusRealModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver() // step over recursive plus call
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    debugger.stepOut() // step out of non-rec plus call
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  // step over function call
  test("step over function call of non-recursive function") {
    val compiledExample = compile(Code.incModule)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step over nested function call of non-recursive function") {
    val code =
      s"""module M
        |def inc(x: Int): Int = x + 1
        |@main def main(): Int = inc(inc(0))
        |""".stripMargin
    val compiledExample = compile(code)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step over function call with multiple arguments as calls") {
    val code =
      s"""module M
        |def add(x: Int, y: Int): Int = x + y
        |@main def main(): Int = add(add(1, 2), add(3, add(4, 5)))
        |""".stripMargin
    val compiledExample = compile(code)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step over nested function calls") {
    val compiledExample = compile(twoFunctionCallArgs)
    val debugger = initDebugger(compiledExample)
    debugger.entry("main", q"Succ(Succ(Zero()))", q"Succ(Zero())")
    debugger.stepInto()
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    println(debugger.currentDebuggerInfo())
    assert(!debugger.isFinished)
    debugger.stepOver()
    assert(!debugger.isFinished)
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    assert(!debugger.isFinished)
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    assert(!debugger.isFinished)
    println(debugger.currentDebuggerInfo())
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("XXYZ") {
    val v = Seq(1, 2, 3, 4).take(36)
    println(v)
  }
}
