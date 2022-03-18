package inca.frontend.functional.integration

import inca.examples.functional.Code
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.functional.executor.IncrementalFunctionalExecutor._
import inca.runtime.EnginePool
import org.scalatest.BeforeAndAfterEach

import scala.meta.quasiquotes._

class IncrementalTests extends AnyFunSuite with BeforeAndAfterEach {
  val trackedRelsPlus = Set("plus", "input$plus")
  val trackedRelsFact = Set("main", "fact", "input$fact")

  override def afterEach(): Unit = {
    EnginePool.disposeAllEngines()
  }

  test("primitive prog increase numerical input") {
    val original = Seq(q"5")
    val changed = Seq(q"8")
    testIncrementalRun(Code.factModule, "main", original, changed, trackedRelsFact)
  }

  test("primitive prog decrease numerical input") {
    val original = Seq(q"8")
    val changed = Seq(q"5")
    testIncrementalRun(Code.factModule, "main", original, changed, trackedRelsFact)
  }

  test("Non-cyclic data change first argument (simple dependency)") {
    val original = Seq(q"Succ(Succ(Succ(Zero())))", q"Succ(Zero())")
    val changed = Seq(q"Succ(Succ(Succ(Succ(Zero()))))", q"Succ(Zero())")
    testIncrementalRun(Code.plusNoMainModule, "plus", original, changed, trackedRelsPlus)
  }

  test("Non-cyclic data change first argument bigger example (simple dependency)") {
    val original = Seq(q"Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero()))))))))))", q"Succ(Zero())")
    val changed = Seq(q"Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Succ(Zero())))))))))))", q"Succ(Zero())")
    testIncrementalRun(Code.plusNoMainModule, "plus", original, changed, trackedRelsPlus)
  }

  test("Non-cyclic data change second argument (requires unrolling of result)") {
    val original = Seq(q"Succ(Succ(Succ(Zero())))", q"Succ(Zero())")
    val changed = Seq(q"Succ(Succ(Succ(Zero())))", q"Succ(Succ(Succ(Zero())))")
    testIncrementalRun(Code.plusNoMainModule, "plus", original, changed, trackedRelsPlus)
  }

  test("Non-cyclic data change second argument bigger example (requires unrolling of result)") {
    val original = Seq(q"Succ(Succ(Succ(Zero())))", q"Succ(Zero())")
    val changed = Seq(q"Succ(Succ(Succ(Zero())))", q"Succ(Succ(Succ(Succ(Succ(Zero())))))")
    testIncrementalRun(Code.plusNoMainModule, "plus", original, changed, trackedRelsPlus)
  }


  def testIncrementalRun(code: String, mainFun: String, original: Seq[meta.Term], changed: Seq[meta.Term], trackedRelations: Set[String] = Set()): Unit = {
    val compiled = compileFunction(code)

    for (i <- 0 until 0) {
      val fun = loadFunction(compiled)
      val (edits, tuple) = fun.input(original)
      val (load, insert, delete, m0) = fun.measureInitial(mainFun, edits, tuple)
      println(s"Initial ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
    }
    EnginePool.disposeAllEngines()


    val fun = loadFunction(compiled)

    val (edits, tuple) = fun.input(original)
    val (load, insert, delete, m0) = fun.measureInitial(mainFun, edits, tuple)
    println(s"Initial ${load / 1000 / 1000}, ${insert / 1000 / 1000}, ${delete / 1000 / 1000}")
    //    IncrementalFunctionalExecutor.printChanges()
    println()

    fun.registerTrackedRelations(trackedRelations)

    for (i <- 0 until 1) {
      // incremental measurement
      val (edits1, tuple1) = fun.input(changed)
      edits1.print()
      val (load1, insert1, delete1, m1) = fun.measureUpdate(mainFun, edits1, tuple1)
      println(s"Change1 ${load1 / 1000 / 1000}, ${insert1 / 1000 / 1000}, ${delete1 / 1000 / 1000}")
      fun.deepPrintChanges()
      println()

      // measure revert of change
      val (edits2, tuple2) = fun.input(original)
      val (load2, insert2, delete2, m2) = fun.measureUpdate(mainFun, edits2, tuple2)
      println(s"Change2 ${load2 / 1000 / 1000}, ${insert2 / 1000 / 1000}, ${delete2 / 1000 / 1000}")
      fun.deepPrintChanges()
      println()
    }

    EnginePool.disposeAllEngines()
  }

}

