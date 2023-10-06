package inca.base

import inca.backend.lowering.{GeneratePSystem, PSystem}
import inca.backend.optimize.EliminateAliases
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.{Body, Eq, Language, Module, Neq, Param, Relation, Var}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.arithmetic
import inca.ir.string2name
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.ScalaCompiler
import inca.runtime
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory

class BaseTest extends AnyFunSuiteLike:
  // Viatra does not like this
  test("Failing body") {
    var mod = Module("Test", Language(arithmetic.IR), Seq(
      Relation("main", Seq(Param("x", TInt)), Seq(
        Body(
          Seq(
            Eq(Var("x"), IntNum(1)),
            Neq(Var("x"), Var("x"))
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

    val res = runtime.Relation.fromMatcher(mainMatcher)
  }
