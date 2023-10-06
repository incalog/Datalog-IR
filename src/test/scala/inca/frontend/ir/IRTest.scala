package inca.frontend.ir

import inca.backend.hints.MagicSetHints
import inca.frontend.ir.{Datalog => DatalogAPI}
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{IntLiteral, Name}
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel
import org.scalatest.funsuite.AnyFunSuite


class IRTest extends AnyFunSuite {
  lazy val dummyModule: CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq()
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = true
      override def withOptimizations(opts: Seq[Optimization]): Options = ???
      override def withTransformations(trans: Seq[Transformation]): Options = ???
    }

    override def name: Name = "Test"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Datalog.Module = Datalog.Module("Test", Seq(), Seq(
      Datalog.Pattern(None, "dummy", Seq(
        Datalog.Param("ret$0", Datalog.TScalaBoolean),
          Datalog.Param("ret$1", Datalog.TScalaBoolean)
      ), Seq(
        Datalog.Body(Seq(
          Datalog.Eq(Datalog.Var("ret$0"), Datalog.True),
          Datalog.Eq(Datalog.Var("ret$1"), Datalog.False))
        ),
        Datalog.Body(Seq(
          Datalog.Eq(Datalog.Var("ret$1"), Datalog.True),
          Datalog.Eq(Datalog.Var("ret$0"), Datalog.False))
        )
      ))
    ), Seq())

    override def dataModel: DataModel = {
      new DataModel()
    }
  }

  lazy val pathModule: CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq()
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = true
      override def withOptimizations(opts: Seq[Optimization]): Options = ???
      override def withTransformations(trans: Seq[Transformation]): Options = ???
    }

    override def name: Name = "Test"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def ir: Datalog.Module = Datalog.Module("Test", Seq(), Seq(
      Datalog.Pattern(None, "path", Seq(
        Datalog.Param("start", Datalog.TScalaString),
        Datalog.Param("end", Datalog.TScalaString)
      ), Seq(
        Datalog.Body(Seq(
          Datalog.ExtensionalCall("node", Seq(Datalog.Var("start"), Datalog.Var("end")))
        )),
        Datalog.Body(Seq(
          Datalog.ExtensionalCall("node", Seq(Datalog.Var("start"), Datalog.Var("mid"))),
          Datalog.Call("path", Seq(Datalog.Var("mid"), Datalog.Var("end")))
        ))
      ))
    ), Seq())

    override def dataModel: DataModel = {
      new DataModel()
    }
  }

  test("Dummy Example 1") {
    val dummyDatalog: DatalogAPI = new DatalogAPI(dummyModule)
    val resRel = dummyDatalog.read(UnitRelation("dummy"))
    assert(resRel.toSet == Set((true, false), (false, true)))
  }

  test("Dummy Example 2") {
    val dummyDatalog: DatalogAPI = new DatalogAPI(dummyModule)
    val resRel = dummyDatalog.read(Relation1("dummy", Seq("ret$0"), Seq(Seq(true))))
    assert(resRel.toSet == Set((true, false)))
  }


  val pathEDB: EDBChange = EDBChange.insertions(
    Seq(
      Relation2("node", Seq("start", "end"), Seq(
        Seq("X", "Y"), Seq("Y", "Z"), Seq("Z", "W"), Seq("W", "Y")
      ))
    )
  )

  test("Path Example 1") {
    val pathDatalog: DatalogAPI = new DatalogAPI(pathModule)
    pathDatalog.update(pathEDB)
    val resRel = pathDatalog.read(Relation1("path", Seq("end"), Seq(Seq("W"))))
    assert(resRel.toSet == Set(("X", "W"), ("Y", "W"), ("Z", "W"), ("W", "W")))
  }

  test("Path Example 2") {
    val pathDatalog: DatalogAPI = new DatalogAPI(pathModule)
    pathDatalog.update(pathEDB)
    val resRel = pathDatalog.read(Relation2("path", Seq("start", "end"), Seq(Seq("X", "W"))))
    assert(resRel.toSet == Set(("X", "W")))
  }
}