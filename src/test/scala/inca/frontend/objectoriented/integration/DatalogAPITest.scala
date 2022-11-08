package inca.frontend.objectoriented.integration

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.{CompiledModule, Compiler, Options, SourceLocation}
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.{Executor, ObjectExecutor}
import inca.frontend.objectoriented.integration.core.GenericTest
import inca.frontend.runner
import inca.frontend.runner.{EDBChange, IRInput, IRRunnerFactory, Relation, Relation1, Relation2}
import inca.runtime.{EnginePool, Query}
import inca.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory


class DatalogAPITest extends GenericTest {
  val executor: Executor = ObjectExecutor

  test("Datalog API Examples") {
    /*val code =
      """def someMethod(): Int {
          return 3
      }
      """.stripMargin
    val module = Compiler.compileObject(code, ObjectOptions())*/

    val module = new CompiledModule {
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
        /*Datalog.Pattern(None, "ret", Seq(
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
        ))*/
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

    val irFactory = new IRRunnerFactory(module)
    val runner = irFactory.runner("path")
    runner.update(
      IRInput(EDBChange.insertions(
        Map(
          "node" -> Relation2("node", Seq("start", "end"), Seq(
            Seq("X", "Y"), Seq("Y", "Z"), Seq("Z", "W"), Seq("W", "Y")
          ))
        )
      ))
    )
    val res = runner.run(IRInput(Relation1("path", "end", Seq(Seq("W")))))
    val res2 = runner.run(IRInput(Relation2("path", Seq("start", "end"), Seq(Seq("X", "W")))))
    println("Result: ", res)
    println("Result: ", res2)
  }
}