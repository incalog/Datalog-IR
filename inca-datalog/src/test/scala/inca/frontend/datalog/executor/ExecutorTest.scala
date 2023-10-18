package inca.frontend.datalog.executor

import inca.frontend.datalog.compile.GenerateIR
import inca.frontend.datalog.executor.DatalogExecutor.?
import inca.frontend.datalog.syntax.*
import inca.frontend.datalog.typecheck.Typechecker
import inca.ir.typing.IRTypechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class ExecutorTest extends AnyFunSuite:
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

  /*test("ShortestPath") {
    val m =
      s"""Edge(Int, Int, Int).
         |Edge(1,2,20).
         |
         |SPath(Int, Int, Int).
         |SPath(X,Y,min(n)) :- Edge(X,Y,n)
         |                  :- Edge(X,Z,n1), SPath(Z,Y,n2), n == n1 + n2.
         |""".stripMargin
    compile(Parser.module.parseAll(m).getOrElse(???))
  }*/
