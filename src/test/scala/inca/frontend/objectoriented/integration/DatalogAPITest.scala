package inca.frontend.objectoriented.integration

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Name
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.{CompiledModule, Compiler, Options, SourceLocation}
import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.datalog_api.Datalog
import inca.frontend.objectoriented.executor.{Executor, ObjectExecutor}
import inca.frontend.objectoriented.integration.core.GenericTest
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory


class ConcreteDatalog(override val compiled: CompiledModule) extends Datalog {
  val scope = new QueryScope(compiled.dataModel)
  val (engine, feed) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
}

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
        Datalog.Pattern(None, "ret", Seq(
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

    val datalog = new ConcreteDatalog(module)
    val res = datalog.query("ret")()
    println("Result: ", res)
  }
}