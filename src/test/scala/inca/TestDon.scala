package inca

import inca.analyzedLangs.Exp._
import inca.compiler.{Compiler, Options}
import inca.debugger.Debugger
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

object TestDon extends App {
  val ast = Add(Add(IntegerLit(2), IntegerLit(3)), IntegerLit(5))
  val ast2 = Add(Add(IntegerLit(7), IntegerLit(3)), IntegerLit(5))

  val analysisCode =
    s"""module ExpLhs
       |
       |def lhs(e: $expTag): $expTag = {
       |  val add = e:$addTag
       |  val vv = foo(add)
       |  yield vv
       |} union {
       |  val mul = e:$multTag
       |  yield mul.lhs
       |}
       |
       |def foo(aa: $addTag): $expTag = {
       |  yield aa.rhs
       |}
       |
       |""".stripMargin

  val options = Options(languageMetaInfo)
  val module = Compiler.compileFun(analysisCode, options)
  val fun = "foo"

  val scope = new QueryScope(languageMetaInfo)
  val psystem = compiler.Compiler.compileGP(module.ir, options).psystemModule
  val querySpec = psystem.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}."))

  val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val matcher = EnginePool.loadQuery(querySpec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  val debugger = new Debugger(feed)

  val editScript = Diffable.load(ast)
  feed.processEditScript(editScript)

  println(ast.toStringWithURI)
  println(debugger.printMatches(matcher))

//  println(ast2.toStringWithURI)
//  val (diff, ast3) = ast.compareTo(ast2)
//  feed.processEditScript(diff)
//
//  println(ast3.toStringWithURI)
//  println(debugger.printMatches(matcher))

}
