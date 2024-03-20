package inca.frontend.datalog.executor

import inca.frontend.datalog.compile.DatalogCompilerOptions
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.ascent.backend.Executor
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class DatalogAscentExecutorTest extends AnyFunSuite:
  val pipeline = List()
  val options = DatalogCompilerOptions.fromResource("datalog/Options.ini")
  val exec: DatalogExecutor = new DatalogExecutor(Executor())

  test("Path") {
    val code = FileUtil.readFileFromResource("datalog/unittests/Path.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("Path", Seq(?, ?))
    assertResult(11)(res.size)

    res = loaded.query("Path", Seq(?, 5), Seq(3, ?))
    assertResult(4)(res.size)

    res = loaded.query("Path", Seq(1, ?))
    assertResult(4)(res.size)

    res = loaded.query("Path", Seq(2, 5))
    assertResult(1)(res.size)
  }

  // recursive Aggregation is not supported
  /*test("ShortestPath") {
    val code = FileUtil.readFileFromResource("datalog/unittests/ShortestPath.dl")
    val compiled = exec.compileDatalog(code)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    // TODO: Double aggregation bug
    var res = loaded.query("SPath", (1, 4, ?))
    println(res)

    res = loaded.query("SPathExternal", (1, 4, ?))
    assertResult((1, 4, 7))(res.entries.head)

    res = loaded.query("SPathExternal", (1, ?, ?))
    assertResult(Set((1, 4, 7), (1,2,4), (1,3,9)))(res.entries.toSet)
  }*/

  test("Sum Aggregation") {
    val code = FileUtil.readFileFromResource("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeSum", Seq("A", ?))
    assertResult(("A", 7))(res.entries.head)

    res = loaded.query("NodeSum", Seq(?, ?))
    assertResult(Set(("A", 7), ("B", 12), ("C", 5)))(res.entries.toSet)
  }

  test("Max Aggregation") {
    val code = FileUtil.readFileFromResource("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeMax", Seq("A", ?))
    assertResult(("A", 4))(res.entries.head)

    res = loaded.query("NodeMax", Seq(?, ?))
    assertResult(Set(("A", 4), ("B", 10), ("C", 5)))(res.entries.toSet)
  }

  test("Min Aggregation") {
    val code = FileUtil.readFileFromResource("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeMin", Seq("A", ?))
    assertResult(("A", 3))(res.entries.head)

    res = loaded.query("NodeMin", Seq(?, ?))
    assertResult(Set(("A", 3), ("B", 2), ("C", 5)))(res.entries.toSet)
  }

  test("Count Aggregation") {
    val code = FileUtil.readFileFromResource("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code, options)
    compiled.setPipeline(pipeline)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeCount", Seq("A", ?))
    assertResult(("A", 2))(res.entries.head)

    res = loaded.query("NodeCount", Seq(?, ?))
    assertResult(Set(("A", 2), ("B", 2), ("C", 1)))(res.entries.toSet)
  }