package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLExecutorTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  // Unittests
  test("Add") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Add.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("OID(Succ,7)")(res.entries.head.toString)
  }

  test("Assignment") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Assignment.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(1)(res.entries.head)
  }

  test("Base") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Base.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("Dynamic Dispatch") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/DynamicDispatch.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult("BBC")(res.entries.head)
  }

  /*
  // TODO: Eliminate aliases to make this work
  test("Equals") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Equals.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }*/

  test("Factorial") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Fact.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fibonacci") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Fib.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(10))
    assertResult(55)(res.entries.head)
  }

  // We need more optimizations to execute the full program
  //  + Disjunction lowering is way to slow on this (is there an endless loop?)
  test("InstanceOf") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/InstanceOf.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Method Inheritance") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/MethodInheritance.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(3)(res.entries.head)
  }

  test("Mutability") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Mutability.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Null") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Null.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Param Object") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/ParamObject.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  // We need way more optimizations to make this program executable
  test("Plus") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Plus.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Subtyping") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Subtyping.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(6)(res.entries.head)
  }

  test("Super") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Super.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(5)(res.entries.head)
  }

  test("Tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Tuple.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(Set((1, 1, 1)))(res.entries.toSet)
  }

  test("TypeCast") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/TypeCast.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("TypeCast failure") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/TypeCastFail.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val caught = intercept[TypeCastException] {
      loaded.execute("main", Seq())
    }
  }

  /** If */

  test("If") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/If.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(11)(res.entries.head)
  }

  test("If Duplicate") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfDuplicate.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq(1, 1))
    assertResult(10)(res.entries.head)
  }

  test("If False") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfFalse.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("If True") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/if/IfTrue.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  /** Case class */

  test("Case class") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/caseclass/CaseClass.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(15)(res.entries.head)
  }

  test("Transitive closure") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/caseclass/TransitiveClosure.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TString_TString$$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1, 3)
    val expectedResult = Set(
      ("A", "W"), ("Z", "Y"), ("Y", "W"), ("B", "C"),
      ("Y", "Z"), ("X", "Z"), ("Y", "Y"), ("Z", "Z"),
      ("X", "Y"), ("B", "W"), ("X", "X"), ("Z", "W"),
      ("Y", "X"), ("X", "W"), ("B", "A"), ("Z", "X")
    )
    assertResult(expectedResult)(res.toSet)
  }

  /** Set */

  test("Set with Objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClass.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(3, 10))(res.toSet)
  }

  test("Set comprehension") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TInt_TInt$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set((1,1), (2,1)))(res.toSet)
  }

  test("Set comprehension 2") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension2.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TInt_TInt_TInt$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 4)
    assertResult(Set((1, 3, 5), (1, 4, 5)))(res.toSet)
  }

  test("Set comprehension 3") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension3.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TInt_TInt$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set((1, 3), (1, 4), (2, 3), (2, 4)))(res.toSet)
  }

  test("Set comprehension Tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehensionTuple.oodl")
    val compiled = exec.compileOODL(code)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TString_TInt$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set(("A", 2), ("C", 2)))(res.toSet)
  }

  // TODO: Currently not supported, need more optimizations (how should equality on objects being handled ?)
  //  Maybe negated destruct as well ?
  /*test("Equals") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/Equals.oodl")
    val compiled = exec.compileOODL(code)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }*/
