package inca.frontend.datamodel

import inca.caseStudies.typing.{Exp, Prog}
import inca.caseStudies.typing.ExpTyping.{Add, App, ExpT, IntLit, Lam, Let, Var}
import inca.compiler.{Compiler, Options}
import truediff.compat.treesitter.GoLang

object SimpleChecks extends App {
  val options = Options(Prog.languageMetaInfo)
  val analysis =
    s"""
       |module DataModelTest
       |
       |datamodel treesitter ./src/test/scala/inca/frontend/datamodel/metainfos/
       |
       |def checkSimple(e: _expression): _expression = {
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
    """func hello(x float64) { "abc" }
      |""".stripMargin

  val tree = goParser.parse(code)
  println(tree)
  println(tree.toStringWithURI)
  goParser.destroy()

}
