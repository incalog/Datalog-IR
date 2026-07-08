package inca.codeql.executor

import org.scalatest.funsuite.AnyFunSuite

class CodeQlJavaEdbDumperTest extends AnyFunSuite:

  test("dump raw CodeQL Java EDB relations"):
    val javaCode =
      """
        |public class GeneratedClass {
        |  public static void main(String[] args) {
        |    int[] xs = new int[10];
        |    System.out.println(xs.length);
        |  }
        |}
        |""".stripMargin

    val csvByRelation =
      CodeQlJavaEdbDumper.dumpAndPrint(
        javaCode,
        Seq(
          "classes_or_interfaces",
          "arrays",
          "exprs",
          "callableEnclosingExpr"
        )
      )

    assert(csvByRelation.contains("files"))
    assert(csvByRelation.contains("arrays"))