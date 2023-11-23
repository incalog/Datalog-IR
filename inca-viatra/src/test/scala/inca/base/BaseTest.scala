package inca.base

import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.{Body, Call, Eq, Language, Module, Neq, Param, Relation, Var, execution, string2name}
import inca.util.{FileUtil, ScalaCompiler}
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuiteLike


class BaseTest extends AnyFunSuiteLike:

  /*test("Failing body") {
    var mod = Module("Test", Language(arithmetic.IR), Seq(
      Relation("main", Seq(Param("x", TInt)), Seq(
        Body(
          Seq(
            // Viatra does not like this and will fail
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

  }*/

  test("Test path") {
    val mod = Module("Path", Language(arithmetic.IR), Seq(
      Relation("edge", Seq(Param("x", TInt), Param("y", TInt)), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(3)),
          Eq(Var("y"), IntNum(4))
        ))
      )),
      Relation("path", Seq(Param("x", TInt), Param("y", TInt)), Seq(
        Body(
          Seq(
            Call("edge", Seq(Var("x"), Var("y")))
          )
        ),
        Body(
          Seq(
            Call("path", Seq(Var("x"), Var("z"))),
            Call("path", Seq(Var("z"), Var("y")))
          )
        ),
      ))
    ))

    var code = GeneratePSystem.compileModules(Seq(mod), false)
    code = s"$code; Path"

    val compiler = new ScalaCompiler()
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
    val pathSpec = psystemModule.patterns("path")()

    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val mainMatcher = engine.getMatcher(pathSpec)

    import scala.jdk.CollectionConverters.*

    val res = execution.Relation.fromMatches(
      mainMatcher.getPatternName,
      mainMatcher.getParameterNames.asScala.toList,
      mainMatcher.getAllMatchArrays.map(_.toSeq))
    assertResult(6)(res.entries.size)
  }

  /*test("Test Plus from File") {
    var code = FileUtil.readFile("code.scala")
    code = s"$code; Plus"

    val compiler = new ScalaCompiler()
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
    val pathSpec = psystemModule.patterns("main")()

    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val mainMatcher = engine.getMatcher(pathSpec)
    feed.insertExtensionalTuple("ext_main$input", Tuples.flatTupleOf())

    import scala.jdk.CollectionConverters.*

    val res = execution.Relation.fromMatches(
      mainMatcher.getPatternName,
      mainMatcher.getParameterNames.asScala.toList,
      mainMatcher.getAllMatchArrays.map(_.toSeq))
    println(res.asTable)
  }*/
