package inca.frontend.datalog.executor

import inca.frontend.datalog.compile.GenerateIR
import inca.frontend.datalog.executor.DatalogExecutor.__
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

    var res = loaded.read("Path", (__, __))
    assertResult(res.size)(11)

    res = loaded.read("Path", (__, 5))
    assertResult(res.size)(3)

    res = loaded.read("Path", (1, __))
    assertResult(res.size)(4)

    res = loaded.read("Path", (2, 5))
    assertResult(res.size)(1)
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
