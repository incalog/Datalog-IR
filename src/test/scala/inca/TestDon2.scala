package inca

import inca.analyzedLangs.Exp._
import inca.compiler.{Compiler, Options}
import inca.runtime.EnginePool
import inca.runtime.context.QueryScope
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import truediff.Diffable

object TestDon2 extends App {
  val ast = Add(Add(IntegerLit(1), IntegerLit(2)), IntegerLit(3))
  // num of exp: 5
  // num of add: 2
  // num of intlit: 3

  val analysisCode =
    s"""module ExpTst
       |
       |def tst(exp: $expTag): $expTag = {
       |  // exp is (exp1, exp2, exp3, exp4, exp5)
       |  vals vs <- $expTag
          // vs is (add1, add2)
          val lhs = s.lhs

       |  yield vs
       |}
       |""".stripMargin // TODO ASK: Behavior of L.18, L.17 on "in" -- DONE

  val options = Options(languageMetaInfo)
  val module = Compiler.compileFun(analysisCode, options)
  val fun = "tst"

  val scope = new QueryScope(languageMetaInfo)
  val psystem = compiler.Compiler.compileGP(module.ir, options).psystemModule
  val querySpec = psystem.patterns.getOrElse(fun, throw new IllegalArgumentException(s"Function $fun undefined in module ${module.name}."))

  val feed = EnginePool.loadDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  val matcher = EnginePool.loadQuery(querySpec(), scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)

  val editScript = Diffable.load(ast)
  feed.processEditScript(editScript)

  println(matcher.countMatches()) // TODO ASK: 10?? -- DONE
  println(matcher.getParameterNames)
  println(matcher.getPatternName)

  val matIt = matcher.getAllMatchArrays
  for(elem <- matIt) println(elem.mkString("(", ", ", ")"))

}
