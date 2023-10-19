package inca.base

import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.{Body, Eq, Language, Module, Neq, Param, Relation, Var, execution, string2name}
import inca.util.ScalaCompiler
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuiteLike

@Ignore
class BaseTest extends AnyFunSuiteLike:
  // Viatra does not like this
  test("Failing body") {
    var mod = Module("Test", Language(arithmetic.IR), Seq(
      Relation("main", Seq(Param("x", TInt)), Seq(
        Body(
          Seq(
            Eq(Var("x"), IntNum(1)),
            Neq(Var("x"), IntNum(1))
          )
        )
      ))
    ))

    //mod = EliminateAliases.optimizer().visit(mod)

    var code = GeneratePSystem.compileModules(Seq(mod))
    code = s"$code; Test"
    //println(code)

    val compiler = new ScalaCompiler()
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
    val mainSpec = psystemModule.patterns("main")()

    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val mainMatcher = engine.getMatcher(mainSpec)
    //val res = mainMatcher.getAllMatches

    import scala.jdk.CollectionConverters.*

    val res = execution.Relation.fromMatches(
      mainMatcher.getPatternName,
      mainMatcher.getParameterNames.asScala.toList,
      mainMatcher.getAllMatchArrays.map(_.toSeq))

  }
