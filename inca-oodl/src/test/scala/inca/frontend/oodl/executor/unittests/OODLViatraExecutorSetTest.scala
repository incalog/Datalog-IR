package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorSetTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  test("Set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/Set.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Set as param") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetAsParam.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Set with Objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClass.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(3, 10))(res.toSet)
  }

  test("Set with Objects 2") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClass2.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(10, 5))(res.toSet)
  }

  test("Simple Set with Objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClassSimple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2))(res.toSet)
  }

  test("Set with tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClassTuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TInt_TString$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set((1, "A"), (2, "B"), (3, "C")))(res.toSet)
  }

  test("Set comprehension") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension.oodl")
    val compiled = exec.compileOODL(code, options)
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
    val compiled = exec.compileOODL(code, options)
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
    val compiled = exec.compileOODL(code, options)
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
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TString_TInt$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set(("A", 2), ("C", 2)))(res.toSet)
  }

  test("Set comprehension Unit") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehensionUnit.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1,2,3))(res.toSet)
  }

  test("Constant set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetConst.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Constant set with variables") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetConstVar.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Empty set as field") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetEmpty.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1))(res.toSet)
  }

  // TODO: Not working, I think something in the set lowering is wrong / missing
  //  Are we handling casting of sets correctly ?
  /*test("Constant empty set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetEmptyConst.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.size)
    //val setAdt = res.entries.head
    //val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    //res = loaded.engine.read(query).project(1, 2)
    //assertResult(Set())(res.toSet)
  }

  test("Empty set as variable") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetEmptyVar.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1))(res.toSet)
  }*/

  test("Set as field") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetFieldDeclare.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Mutate set as field") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetFieldSet.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3))(res.toSet)
  }

  test("Set if") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetIf.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq(0))
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 3, 4))(res.toSet)
  }

  test("Set intersection") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetIntersect.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(3))(res.toSet)
  }

  test("Set method nested") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetMethodNested.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2))(res.toSet)
  }

  test("Set recursive") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetRecursive.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TString$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set("X", "Y", "Z", "W"))(res.toSet)
  }

  test("Set with tuple elements") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetTuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TInt_TString$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 3)
    assertResult(Set((1, "A"), (2, "B"), (3, "C")))(res.toSet)
  }

  test("Set union") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetUnion.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1,2,3,4))(res.toSet)
  }

  test("Set union with objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetUnionObject.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assertResult(Set(1, 2, 3, 4))(res.toSet)
  }