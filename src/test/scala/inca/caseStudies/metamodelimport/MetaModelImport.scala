package inca.caseStudies.metamodelimport

import inca.caseStudies.typing.Exp
import inca.compiler.{Compiler, Options}

object ExpTyping extends App {
  val options = Options(Exp.languageMetaInfo)
  val code =
    s"""
       |module SimpleChekcks
       |
       |metamodelpath ./src/test/scala/inca/analyzedLangs/
       |metamodel STL
       |
       |`import inca.caseStudies.typing.Type`
       |`import inca.caseStudies.typing.Context`
       |
       |def checkSimple(e: expression): `Type` = e match {
       |  case boolean => yield `Type.Int`
       |  case e1 and e2) =>
       |    if (checkSimple(e1) == `Type.Int` &&
       |        checkSimple(e2) == `Type.Int`)
       |      yield `Type.Bool`
       |    else
       |      fail
       |}
       |""".stripMargin

  val compiled = Compiler.compileFun(code, options)
  println(compiled.optimized)

}
