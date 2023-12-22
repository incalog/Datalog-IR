package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  // Unittests
  test("Add") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Add.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("OID(Succ,7)")(res.entries.head.toString)
  }

  test("Assignment") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Assignment.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(1)(res.entries.head)
  }

  test("Base") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Base.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Dynamic Dispatch") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/DynamicDispatch.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("BBC")(res.entries.head)
  }

  /*
  // TODO: Eliminate aliases to make this work
  test("Equals") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Equals.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }*/

  test("Factorial") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Fact.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fibonacci") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Fib.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(10))
    assertResult(55)(res.entries.head)
  }

  test("Fix method") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/FixMethod.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    val fixMethodRel = loaded.engine.read(UnitRelation("fixMethod$Tuple_"))
    assertResult(true)(fixMethodRel.entries.nonEmpty)
  }

  // We need more optimizations to execute the full program
  //  + Disjunction lowering is way to slow on this (is there an endless loop?)
  test("InstanceOf") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/InstanceOf.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Method Inheritance") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/MethodInheritance.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(3)(res.entries.head)
  }

  test("Mutability") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Mutability.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Null") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Null.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Param Object") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/ParamObject.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  // We need way more optimizations to make this program executable
  test("Plus") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Plus.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Subtyping") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Subtyping.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }

  test("Super") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Super.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(5)(res.entries.head)
  }

  test("Tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Tuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(Set((1, 1, 1)))(res.entries.toSet)
  }

  test("TypeCast") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/TypeCast.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("TypeCast failure") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/TypeCastFail.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val caught = intercept[TypeCastException] {
      loaded.execute("main", Seq())
    }
  }
