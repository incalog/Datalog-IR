package inca.frontend.functionaxsouffle

import inca.frontend.functionalxsouffle.executor.FunctionalXSouffleExecutor
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionalFrontendSouffleIntegrationTest extends AnyFunSuite {

  // TODO Currently we only support legacy souffle
  val funCode =
    s"""module SimpleProg
       |@main def main(x: Int): Set[Int] = {y | (x, y) in path}
       |
       |""".stripMargin

  val souffleCode =
    s""".decl edge(?in: number, ?out: number)
       |.input edge(IO="file", filename="edge.facts", delimiter="\t")
       |
       |.decl path(?in: number, ?out: number)
       |.output path
       |
       |path(x, y) :- edge(x, y).
       |path(x, y) :- path(x, z), edge(z, y).
       |""".stripMargin

  test("import simple Souffle program") {
    val fun = FunctionalXSouffleExecutor.loadFunction(funCode, souffleCode)

    val res = fun.execute("main", Seq(q"1"), Map("edge" -> Seq(Seq("1", "2"), Seq("2", "4"), Seq("2", "1"), Seq("3", "4"))), false)

    println(res)
  }
}
