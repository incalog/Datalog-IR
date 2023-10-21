package inca.frontend.datalog.executor

import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class DatalogExecutorTest extends AnyFunSuite:
  val exec: DatalogExecutor = new DatalogExecutor(inca.viatra.Executor)

  test("Path") {
    val code = FileUtil.readFile("datalog/unittests/Path.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("Path", (?, ?))
    assertResult(11)(res.size)

    res = loaded.query("Path", (?, 5), (3, ?))
    assertResult(5)(res.size)

    res = loaded.query("Path", (1, ?))
    assertResult(4)(res.size)

    res = loaded.query("Path", (2, 5))
    assertResult(1)(res.size)
  }

  test("ShortestPath") {
    val code = FileUtil.readFile("datalog/unittests/ShortestPath.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    // TODO: Double aggregation bug
    var res = loaded.query("SPath", (1, 4, ?))
    println(res)

    res = loaded.query("SPathExternal", (1, 4, ?))
    assertResult((1, 4, 7))(res.entries.head)

    res = loaded.query("SPathExternal", (1, ?, ?))
    assertResult(Set((1, 4, 7), (1,2,4), (1,3,9)))(res.entries.toSet)
  }

  test("Sum Aggregation") {
    val code = FileUtil.readFile("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeSum", ("A", ?))
    assertResult(("A", 7))(res.entries.head)

    res = loaded.query("NodeSum", (?, ?))
    assertResult(Set(("A", 7), ("B", 12), ("C", 5)))(res.entries.toSet)
  }

  test("Max Aggregation") {
    val code = FileUtil.readFile("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeMax", ("A", ?))
    assertResult(("A", 4))(res.entries.head)

    res = loaded.query("NodeMax", (?, ?))
    assertResult(Set(("A", 4), ("B", 10), ("C", 5)))(res.entries.toSet)
  }

  test("Min Aggregation") {
    val code = FileUtil.readFile("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeMin", ("A", ?))
    assertResult(("A", 3))(res.entries.head)

    res = loaded.query("NodeMin", (?, ?))
    assertResult(Set(("A", 3), ("B", 2), ("C", 5)))(res.entries.toSet)
  }

  test("Count Aggregation") {
    val code = FileUtil.readFile("datalog/unittests/Aggregate.dl")
    val compiled = exec.compileDatalog(code)
    val loaded = exec.loadDatalog(compiled)

    var res = loaded.query("NodeCount", ("A", ?))
    assertResult(("A", 2))(res.entries.head)

    res = loaded.query("NodeCount", (?, ?))
    assertResult(Set(("A", 2), ("B", 2), ("C", 1)))(res.entries.toSet)
  }