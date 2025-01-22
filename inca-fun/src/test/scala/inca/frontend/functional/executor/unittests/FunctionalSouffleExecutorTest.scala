package inca.frontend.functional.executor.unittests

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.frontend.functional.foreign
import inca.ir.execution.{ADT, Relation}
import inca.souffle.backend.Executor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class FunctionalSouffleExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(Executor())

  // Unittests

  test("Base 1") {
    val code = FileUtil.readFileFromResource("functional/unittests/Base1.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("ADTTest") {
    val code = FileUtil.readFileFromResource("functional/unittests/ADTTest.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(
      ADT("Exp", "Add", Seq(
        ADT("Exp", "Num", Seq(1)),
        ADT("Exp", "Num", Seq(2))
      ))
    ))
    assertResult("$Add($Add($Num(1), $Num(2)), $Num(10))")(res.entries.head.toString)
  }

  test("Fac") {
    val code = FileUtil.readFileFromResource("functional/unittests/Fact.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(120)(res.entries.head)
  }

  test("Fib") {
    val code = FileUtil.readFileFromResource("functional/unittests/Fib.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(7))
    assertResult(13)(res.entries.head)
  }

  test("Inc") {
    val code = FileUtil.readFileFromResource("functional/unittests/Inc.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(1)(res.entries.head)
  }

  test("Unary") {
    val code = FileUtil.readFileFromResource("functional/unittests/Unary.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(5))
    assertResult(-5)(res.entries.head)
  }

  test("Var") {
    val code = FileUtil.readFileFromResource("functional/unittests/Var.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(43)(res.entries.head)
  }

  test("If") {
    val code = FileUtil.readFileFromResource("functional/unittests/If.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(7)(res.entries.head)
  }

  test("If2") {
    val code = FileUtil.readFileFromResource("functional/unittests/If2.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(10)(res.entries.head)
  }

  // TODO: Currently we are not getting back booleans, but ints because of the lowering
  //  Either we want an unlower/lift or we want to support booleans ?
  test("Parametric Eq") {
    val code = FileUtil.readFileFromResource("functional/unittests/ParametricEq.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    var loaded = exec.loadFunction(compiled)
    var res = loaded.execute("main", Seq(1, 1))
    assertResult(1)(res.entries.head)

    // clear the input
    loaded = exec.loadFunction(compiled)
    res = loaded.execute("main", Seq(1, 2))
    assertResult(0)(res.entries.head)
  }

  test("Parametric function") {
    val code = FileUtil.readFileFromResource("functional/unittests/ParametricFunction.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    assertResult((12, 1))(res.entries.head)
  }

  test("Tuple as input") {
    val code = FileUtil.readFileFromResource("functional/unittests/TupleAsInput.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    var loaded = exec.loadFunction(compiled)
    var res = loaded.execute("main", Seq(1, "A"))
    assertResult("A")(res.entries.head)

    loaded = exec.loadFunction(compiled)
    res = loaded.execute("main2", Seq(1, "A"))
    assertResult((1, "A"))(res.entries.head)
  }

  // With our current design main must not be recursive
  /*test("Fix function") {
    val code = FileUtil.readFile("functional/unittests/FixFunction.finca")
    val compiled = exec.compileFunction(code)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(1))
    assertResult(13)(res.entries.head)
  }*/

  // ADT-tests

  test("Plus") {
    val code = FileUtil.readFileFromResource("functional/unittests/Plus.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq(
      ADT("Nat", "Succ", Seq(
        ADT("Nat", "Succ", Seq(
          ADT("Nat", "Succ", Seq(
            ADT("Nat", "Zero", Seq())
          ))
        ))
      ))
    ))
    // TODO: Do not compare by string
    assertResult("$Succ($Succ($Succ($Succ($Succ($Zero)))))")(res.entries.head.toString)
  }

  test("Set const") {
    val code = FileUtil.readFileFromResource("functional/unittests/SetConst.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    var res = loaded.execute("grades", Seq())
    var query = Relation.from("Set$TString$enum", Seq("$elem"), Seq(Seq(null)))
    res = loaded.engine.read(query).project(0)
    assertResult(Set("1.0", "1.3", "1.7", "2.0", "2.3", "2.7", "3.0", "3.3", "3.7", "4.0", "5.0"))(res.toSet)

    res = loaded.execute("flip", Seq())
    query = Relation.from("Set$TInt$enum", Seq("$elem"), Seq(Seq(null)))
    res = loaded.engine.read(query).project(0)
    assertResult(Set(0, 1))(res.toSet)
  }

  test("Set intersection") {
    val code = FileUtil.readFileFromResource("functional/unittests/SetIntersection.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    // Query main to get the set ADT, afterwards query the set relation
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(1, 3))(res.toSet)
  }

  test("Complex set intersection") {
    val code = FileUtil.readFileFromResource("functional/unittests/ComplexSetIntersection.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    var res = loaded.execute("main", Seq())
    var setAdt = res.entries.head
    var query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(3, 4))(res.toSet)

    res = loaded.execute("main2", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(1, 4))(res.toSet)

    res = loaded.execute("main3", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(16))(res.toSet)

    res = loaded.execute("main4", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(10))(res.toSet)

    res = loaded.execute("main5", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(3, 4))(res.toSet)

    res = loaded.execute("main6", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(3, 4, 5))(res.toSet)

    /*res = loaded.execute("main7", Seq())
    setAdt = res.entries.head
    query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query)
    println(res)
    assertResult(Set())(res.toSet)*/
  }

  test("Set Ops") {
    val code = FileUtil.readFileFromResource("functional/unittests/SetOps.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    var res = loaded.execute("union", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$TInt$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1)
    assertResult(Set(0, 1))(res.toSet)
  }

  test("Parametric Datatypes") {
    val code = FileUtil.readFileFromResource("functional/unittests/ParametricDatatypes.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val res = loaded.execute("main", Seq())
    // TODO: Do not compare by string
    assertResult("$Cons_TBoolean(1, $Nil_TBoolean)")(res.entries.head.toString)
  }

// No recursive aggregation support in Souffle + Missing C++ lowering for user-defined aggregation
/*test("Fold Int") {
  val code = FileUtil.readFileFromResource("functional/unittests/FoldInt.finca")
  val compiled = exec.compileFunction(code)
  compiled.setPipeline(CompiledFunctionalUnit.pipeline)
  compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
  val loaded = exec.loadFunction(compiled)
  val res = loaded.execute("sum", Seq(1, 5))
  assertResult(15)(res.entries.head)
}

test("Fold ADT") {
  val code = FileUtil.readFileFromResource("functional/unittests/FoldADT.finca")
  val compiled = exec.compileFunction(code)
  compiled.setPipeline(CompiledFunctionalUnit.pipeline)
  compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
  val loaded = exec.loadFunction(compiled)
  val res = loaded.execute("sum", Seq(1, 5))
  assertResult("V(15)")(res.entries.head.toString)
}

test("Bus Station") {
  val code = FileUtil.readFileFromResource("functional/unittests/BusStation.finca")
  val compiled = exec.compileFunction(code)
  compiled.setPipeline(CompiledFunctionalUnit.pipeline)
  compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
  val loaded = exec.loadFunction(compiled)
  val res = loaded.execute("main", Seq())
  assertResult("BusStation(B,5)")(res.entries.head.toString)
}*/
