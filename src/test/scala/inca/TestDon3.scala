package inca

import inca.analyzedLangs.Exp.{Add, IntegerLit, Mul, addTag, expTag, languageMetaInfo, multTag}
import inca.compiler.{Compiler, Options}
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

object TestDon3 extends App {
  val ast1 = Mul(IntegerLit(9), Add(IntegerLit(5), IntegerLit(7)))
  //            1   2                3 4             5
  //val ast2 = Mul(BoolLit(true), Add(IntegerLit(5), IntegerLit(7)))
  //           6   7                8 9             10
  //val ast3 = Mul(BoolLit(true), Add(IntegerLit(5), IntegerLit(7)))
  //           1   7                3  4             5

  val analysisCode =
    s"""module TestLst
       |
       |def rhs(e: $addTag): $expTag = {
       |  yield e.rhs
       |}
       |
       |""".stripMargin

  val ast2 = Add(Mul(IntegerLit(1), IntegerLit(2)), Mul(IntegerLit(1), IntegerLit(2)))
  val analysisCode2 =
    s"""module ExpLhs
       |
       |def lhs(e: $expTag): $expTag = {
       |  val add = e:$addTag
       |  yield add.lhs
       |} union {
       |  val mul = e:$multTag
       |  foo(mul)
       |  yield mul.lhs
       |}
       |""".stripMargin

  val options = Options(languageMetaInfo)
  val module = Compiler.compileFun(analysisCode2, options)
  val fun = "lhs"

  val scope = new QueryScope(languageMetaInfo)
  val psystem = compiler.Compiler.compileGP(module.ir, options).psystemModule
  val querySpec = psystem.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}."))

  val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val matcher = EnginePool.loadQuery(querySpec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  val editScript = Diffable.load(ast2)
  val (ast3, diffEditscript) = ast1.compareTo(ast2)

  feed.processEditScript(editScript)

  println(matcher.getAllMatches)
  println("Count: " + matcher.countMatches())
  println(matcher.getAllValues("e"))
  println(matcher.getParameterNames)

  val matIt = matcher.getAllMatchArrays
  for(elem <- matIt) println(elem.mkString("(", ", ", ")"))

}

