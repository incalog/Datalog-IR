package inca.codeql.full

import inca.codeql.edb.CodeQlJavaEdbDumper
import inca.codeql.executor.CodeQlExecutor
import inca.codeql.executor.CodeQlExecutor.?
import inca.ir.execution.Relation
import inca.ir.execution.interpreter.Executor
import org.scalatest.funsuite.AnyFunSuite


class CodeQlFullTest extends AnyFunSuite{
  private val codeQl = CodeQlExecutor(Executor())

  test("query for all statements in a java file"):
    val compiled = codeQl.compileCodeQl(
      """
        |import java
        |external predicate wildcards(int c0, string c1, int c2);
        |from
        |  int c0,
        |  string c1,
        |  int c2
        |where
        |  wildcards(c0, c1, c2)
        |select
        |  c0, c1, c2
        |  """.stripMargin
    )
    val javaCode =
      """
        |public class GeneratedClass {
        |  public static void main(String[] args) {
        |    int[] xs = new int[10];
        |    System.out.println(xs.length);
        |  }
        |}
        |""".stripMargin

    val edb = CodeQlJavaEdbDumper.dumpAllMaterialized(
        javaCode
      )
    val loaded = codeQl.loadCodeQl(compiled, edb)
    print(loaded.query("__codeql_select", Seq(?,?,?)).asTable)

    assertResult(353)(loaded.query("__codeql_select", Seq(?,?,?)).size)

}
