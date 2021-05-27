package inca.frontend.datamodel

import inca.executor.ConstraintExecutor
import truediff.compat.treesitter.{GoLang, JavaLang}

object GetLHSJava extends App {
  val analysis =
    s"""
       |module DataModelTest
       |
       |datamodel treesitter ./src/main/resources/tree-sitter-java
       |
       |def getLHS(e: expression): expression = {
       |  if (e.isInstanceOf[binary_expression]) {
       |    val binary = e:binary_expression
       |    yield binary.left
       |  }
       |  else
       |    fail
       |}
       |
       |""".stripMargin


  val javaParser = JavaLang.newParser()

  val code =
    """(1 + 2) * 5
      """.stripMargin

  val tree = javaParser.parse(code)
  println(tree)
  println(tree.toStringWithURI)
  javaParser.destroy()

  val loadedAnalysis = ConstraintExecutor.loadAnalysis(analysis)

  val res1 = loadedAnalysis.execute(tree, "getLHS")
  res1.foreach(println)

}
