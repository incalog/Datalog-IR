package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.CompiledOODLModule
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorSetTest extends AnyFunSuite:
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

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
