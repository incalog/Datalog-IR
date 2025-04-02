package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.{Relation, UnitRelation}
import inca.util.FileUtil
import inca.ir
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorSetTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  def setRelationName(setSignature: Seq[ir.Type]): String =
    val dollars = if (setSignature.size == 1) "$" else "$$"
    s"Set$dollars${setSignature.mkString("_")}${dollars}enum"

  private def verifySet[T](setSignature: Seq[ir.Type], expected: Set[T])(implicit loaded: OODLExecutor#Loaded) =
    val setRelName = setRelationName(setSignature)
    loaded.execute("main", Seq()) // execute main to insert edb tuble
    val res = loaded.engine.read(UnitRelation(setRelName))
    val arity = res.arity
    // start at 1 if the set relation does not include the set object any longer
    val start = if (arity > setSignature.size) 1 else 0
    // drop all irrelevant values
    val actual = res.project(start, start + setSignature.size)
    assertResult(expected)(actual.toSet)

  private def verifyIntSet_1(expected: Set[Int])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irarith.TInt), expected)(using loaded)

  private def verifyIntSet_2(expected: Set[(Int, Int)])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irarith.TInt, irarith.TInt), expected)(using loaded)

  private def verifyIntSet_3(expected: Set[(Int, Int, Int)])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irarith.TInt, irarith.TInt, irarith.TInt), expected)(using loaded)

  private def verifyStringSet_1(expected: Set[String])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irstr.TString), expected)(using loaded)

  private def verifyStringSet_2(expected: Set[(String, String)])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irstr.TString, irstr.TString), expected)(using loaded)


  private def verifyIntStringSet(expected: Set[(Int, String)])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irarith.TInt, irstr.TString), expected)(using loaded)

  private def verifyStringIntSet(expected: Set[(String, Int)])(implicit loaded: OODLExecutor#Loaded) =
    verifySet(Seq(irstr.TString, irarith.TInt), expected)(using loaded)

  test("Set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/Set.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Set as param") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetAsParam.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Set with Objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClass.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(3, 10))(using loaded)
  }

  test("Set with Objects 2") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClass2.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    loaded.execute("main", Seq())
    val setRelName = setRelationName(Seq(irarith.TInt))
    val res = loaded.engine.read(UnitRelation(setRelName))
    assertResult(res.project(0, 1).toSet)(Set(10,5))
  }

  test("Simple Set with Objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClassSimple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2))(using loaded)
  }

  test("Set with tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetClassTuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    loaded.execute("main", Seq())
    val setRelName = setRelationName(Seq(irarith.TInt, irstr.TString))
    val res = loaded.engine.read(UnitRelation(setRelName))
    assertResult(Set((1, "A"), (2, "B"), (3, "C")))(res.project(0, 2).toSet)
  }

  test("Set comprehension") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_2(Set((1, 2), (2, 3)))(using loaded)
  }

  test("Set comprehension 2") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension2.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    loaded.execute("main", Seq())
    val res = loaded.engine.read(UnitRelation("Set$$TInt_TInt_TInt$$enum"))
    assertResult(2)(res.size)
  }

  test("Set comprehension 3") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehension3.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_2(Set((1, 3), (1, 4), (2, 3), (2, 4)))(using loaded)
  }

  test("Set comprehension Tuple") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehensionTuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyStringIntSet(Set(("A", 2), ("C", 2)))(using loaded)
  }

  test("Set comprehension Unit") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetComprehensionUnit.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Constant set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetConst.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Constant set with variables") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetConstVar.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Empty set as field") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetEmpty.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val setAdt = loaded.execute("main", Seq()).entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set"), Seq(Seq(setAdt)))
    val res = loaded.engine.read(query)
    assertResult(res.nonEmpty)
  }

  // TODO: Not working, I think something in the set lowering is wrong / missing
  //  Are we handling casting of sets correctly ?
  /*test("Constant empty set") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetEmptyConst.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
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
    compiled.setPipeline(CompiledOODLUnit.pipeline)
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
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Mutate set as field") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetFieldSet.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3))(using loaded)
  }

  test("Set if") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetIf.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val setAdt = loaded.execute("main", Seq(0)).entries.head
    val query = Relation.from("Set$TInt$enum", Seq("set$0"), Seq(Seq(setAdt)))
    val res = loaded.engine.read(query).project(1)
    assertResult(Set(1,3,4))(res.toSet)
  }

  test("Set intersection") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetIntersect.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val setAdt = loaded.execute("main", Seq()).entries.head
    val query = Relation.from("Set$TInt$enum", Seq("set$0"), Seq(Seq(setAdt)))
    val res = loaded.engine.read(query).project(1)
    assertResult(res.toSet)(Set(3))
  }

  test("Set method nested") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetMethodNested.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    val setAdt = loaded.execute("main", Seq()).entries.head
    val query = Relation.from("Set$TInt$enum", Seq("set$0"), Seq(Seq(setAdt)))
    val res = loaded.engine.read(query).project(1)
    assertResult(res.toSet)(Set(1, 2))
  }

  test("Set recursive") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetRecursive.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyStringSet_1(Set("X", "Y", "Z", "W"))(using loaded)
  }

  test("Set recursive 2") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetRecursive2.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$ID$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)
    assert(res.toSet.nonEmpty)
  }

  test("Set with tuple elements") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetTuple.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntStringSet(Set((1, "A"), (2, "B"), (3, "C")))(using loaded)
  }

  test("Set union") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetUnion.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3, 4))(using loaded)
  }

  test("Set union with objects") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/set/SetUnionObject.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledOODLUnit.optimizationPipeline)
    val loaded = exec.loadOODL(compiled)
    verifyIntSet_1(Set(1, 2, 3, 4))(using loaded)
  }