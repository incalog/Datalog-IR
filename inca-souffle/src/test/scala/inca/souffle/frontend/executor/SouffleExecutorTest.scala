package inca.souffle.frontend.executor

import inca.ir.execution.interpreter.Executor
import inca.souffle.frontend.executor.SouffleExecutor.?
import org.scalatest.funsuite.AnyFunSuite

class SouffleExecutorTest extends AnyFunSuite:
  private val exec = SouffleExecutor(Executor())

  test("compile, load, and query a Souffle program"):
    val compiled = exec.compileSouffle(
      """
        |.decl edge(x:number, y:number)
        |.decl path(x:number, y:number)
        |
        |edge(1, 2).
        |edge(2, 3).
        |path(x, y) :- edge(x, y).
        |path(x, z) :- path(x, y), edge(y, z).
        |.output path
        |""".stripMargin
    )

    val loaded = exec.loadSouffle(compiled)

    assertResult(Set((1, 2), (1, 3)))(loaded.query("path", Seq(1, ?)).toSet)
    assertResult(Set((1, 2), (1, 3), (2, 3)))(loaded.outputs().head.toSet)

  test("reject a query tuple with the wrong arity"):
    val compiled = exec.compileSouffle(
      """
        |.decl value(x:number)
        |value(1).
        |.output value
        |""".stripMargin
    )
    val loaded = exec.loadSouffle(compiled)

    assertThrows[IllegalArgumentException](loaded.query("value", Seq(1, 2)))
