package inca.frontend.oodl.executor

import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLExecutorTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  // Unittests

  test("Base") {
    val code = FileUtil.readFile("objectoriented/unittests/Base.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Factorial") {
    val code = FileUtil.readFile("objectoriented/unittests/Fact.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fibonacci") {
    val code = FileUtil.readFile("objectoriented/unittests/Fib.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(10))
    assertResult(55)(res.entries.head)
  }

  // We need more optimizations to execute the full program
  //  + Disjunction lowering is way to slow on this (is there an endless loop?)
  test("InstanceOf") {
    val code = FileUtil.readFile("objectoriented/unittests/InstanceOf.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Dynamic Dispatch") {
    val code = FileUtil.readFile("objectoriented/unittests/DynamicDispatch.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("BBC")(res.entries.head)
  }

  test("Method Inheritance") {
    val code = FileUtil.readFile("objectoriented/unittests/MethodInheritance.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(3)(res.entries.head)
  }

  // We need way more optimizations to make this program executable
  test("Plus") {
    val code = FileUtil.readFile("objectoriented/unittests/Plus.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Super") {
    val code = FileUtil.readFile("objectoriented/unittests/Super.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(5)(res.entries.head)
  }

  test("Assignment") {
    val code = FileUtil.readFile("objectoriented/unittests/Assignment.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(1)(res.entries.head)
  }

  test("Mutability") {
    val code = FileUtil.readFile("objectoriented/unittests/Mutability.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If") {
    val code = FileUtil.readFile("objectoriented/unittests/if/If.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(11)(res.entries.head)
  }

  test("If Duplicate") {
    val code = FileUtil.readFile("objectoriented/unittests/if/IfDuplicate.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(10)(res.entries.head)
  }

  test("If False") {
    val code = FileUtil.readFile("objectoriented/unittests/if/IfFalse.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If True") {
    val code = FileUtil.readFile("objectoriented/unittests/if/IfTrue.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Case class") {
    val code = FileUtil.readFile("objectoriented/unittests/caseclass/CaseClass.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(15)(res.entries.head)
  }

  // TODO: Currently not supported, need more optimizations (how should equality on objects being handled ?)
  /*test("Equals") {
    val code = FileUtil.readFile("objectoriented/unittests/Equals.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }*/
