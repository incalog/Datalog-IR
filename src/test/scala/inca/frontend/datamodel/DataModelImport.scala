package inca.frontend.datamodel

import inca.executor.ConstraintExecutor
import truediff.compat.treesitter.GoLang

object SimpleChecks extends App {
  val analysis =
    s"""
       |module DataModelTest
       |
       |datamodel treesitter ./src/test/scala/inca/frontend/datamodel/metainfos
       |
       |def getLHS(e: _expression): _expression = {
       |  if (e.isInstanceOf[binary_expression]) {
       |    val binary = e:binary_expression
       |    yield binary.left
       |  }
       |  else
       |    fail
       |}
       |
       |""".stripMargin


  val goParser = GoLang.newParser()
  //  val pyParser = new TSParser("tree-sitter-python")

  val code =
    """(1 + 2) * 5
      """.stripMargin

  val tree = goParser.parse(code)
  println(tree)
  println(tree.toStringWithURI)
  goParser.destroy()

  val loadedAnalysis = ConstraintExecutor.loadAnalysis(analysis)

  val res1 = loadedAnalysis.execute(tree, "getLHS")
  res1.foreach(println)

}
