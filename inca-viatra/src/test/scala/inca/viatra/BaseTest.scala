package inca.viatra

import inca.ir.extension.arithmetic
import inca.ir.extension.string
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.string.{TString, ToString}
import inca.ir.{Body, Call, Eq, Language, Module, Param, Relation, Var, execution, string2name}
import inca.util.{FileUtil, ScalaCompiler}
import inca.viatra.compile.{GeneratePSystem, PSystem}
import inca.viatra.runtime
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.matchers.tuple.Tuples
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.{ term2Arg, termList2ArgList}
import inca.util.compileroptions.CompilerOptions


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
    val mod = Module("Path", Language(arithmetic.IR, string.IR), Seq(
      Relation("edge", Seq(Param("x", TString), Param("y", TString)), Seq(
        Body(Seq(
          Eq(Var("x"), ToString(IntNum(1))),
          Eq(Var("y"), ToString(IntNum(2)))
        )),
        Body(Seq(
          Eq(Var("x"), ToString(IntNum(2))),
          Eq(Var("y"), ToString(IntNum(3)))
        )),
        Body(Seq(
          Eq(Var("x"), ToString(IntNum(3))),
          Eq(Var("y"), ToString(IntNum(4)))
        ))
      )),
      Relation("path", Seq(Param("x", TString), Param("y", TString)), Seq(
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

    val options = CompilerOptions.default

    val viatraLogging = options("viatra_logging")
    viatraLogging.update("typed", false)
    viatraLogging.update("module", true)
    viatraLogging.update("lowerings", false)
    viatraLogging.update("psystem", false)

    val viatraOptions = options("viatra_options")
    viatraOptions.update("apply_double_aggregation_rewrite", true)

    var code = GeneratePSystem.compileModules(Seq(mod), options)
    code = s"$code; Path"

    val compiler = new ScalaCompiler(options)
    val psystemModule: PSystem.Module = compiler.compileAndLoadScala(code)
    val pathSpec = psystemModule.patterns("path")()

    val scope = new QueryScope(new DataModel())
    val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    val mainMatcher = engine.getMatcher(pathSpec)

    import scala.jdk.CollectionConverters.*

    val res = execution.Relation.from(
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
