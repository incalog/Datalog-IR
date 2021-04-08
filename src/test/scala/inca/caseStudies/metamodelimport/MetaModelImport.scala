package inca.caseStudies.metamodelimport

import inca.caseStudies.typing.Exp
import inca.caseStudies.typing.ExpTyping.{Add, App, ExpT, IntLit, Lam, Let, Var}
import inca.compiler.{Compiler, Options}

object SimpleChecks extends App {
  val options = Options(Exp.languageMetaInfo)
  val code =
    s"""
       |module MetamodelTest
       |
       |metamodelpath ./src/test/scala/inca/analyzedLangs/
       |metamodel GoLang
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

  val compiled = Compiler.compileFun(code, options)
  println(compiled.fun)
  println(compiled.typed)
  println(compiled.ir)
  println(compiled.optimized)
  println(compiled.fun.usingMetaModel.get)

}
