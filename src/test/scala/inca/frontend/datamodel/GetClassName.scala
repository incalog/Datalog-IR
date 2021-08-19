package inca.frontend.datamodel

import inca.executor.ConstraintExecutor
import truediff.compat.treesitter.JavaLang

object GetClassName extends App {

  val analysis =
    s"""
       |module FindBugs
       |datamodel treesitter ./src/main/resources/tree-sitter-java
       |
       |def getClassName(class: class_declaration): String = {
       |  val name = class.name._0
       |  yield name
       |}
       |""".stripMargin



  val javaParser = JavaLang.newParser()

  val code =
    """public class FinalClassExample {
            void func() {return 1 + 2}
       }
      """.stripMargin

  val tree = javaParser.parse(code)
  println(tree)
  println(tree.toStringWithURI)
  javaParser.destroy()

 val loadedAnalysis = ConstraintExecutor.loadAnalysis(analysis)

  println(loadedAnalysis.compiled.dataModel)

  val res1 = loadedAnalysis.execute(tree, "getClassName")
  print(res1.toString())

}
