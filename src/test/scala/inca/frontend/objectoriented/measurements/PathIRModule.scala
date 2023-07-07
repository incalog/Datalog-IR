package inca.frontend.objectoriented.measurements

import inca.backend.ir.Datalog
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel

object PathIRModule {
  def line(from: Int, to: Int): Set[Seq[Int]] = {
    if (from < to - 1)
      Set(Seq(from, from + 1)) ++ line(from + 1, to)
    else
      Set(Seq(from, to))
  }

  def loop(from: Int, to: Int): Set[Seq[Int]] = {
    line(from, to) ++ Set(Seq(to, from))
  }

  def cycles(current: Int, step: Int, endNode: Int): Set[Seq[Int]] = {
    if (current < endNode)
      loop(current, current + step) ++ cycles(current + step, step, endNode)
    else
      line(current, endNode) ++ Set(Seq(endNode, current))
  }

  def input(endNode: Int): Set[Seq[Int]] = line(1, 10) ++ loop(10, endNode)

  def inputWithCycle(cycleStep: Int, endNode: Int): Set[Seq[Int]] = line(1, 10) ++ cycles(10, cycleStep, endNode)

  def module(recursive: String): CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq()
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = true
      override def withOptimizations(opts: Seq[Optimization]): Options = throw new RuntimeException("No optimizations")
      override def withTransformations(trans: Seq[Transformation]): Options = throw new RuntimeException("No transformations")
    }

    override def name: Datalog.Name = "Path"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def dataModel: DataModel = new DataModel()

    override def ir: Datalog.Module = Datalog.Module("Path", Seq(), Seq(
      Datalog.Pattern(None, "path",
        Seq(
          Datalog.Param("x", Datalog.TScalaInt),
          Datalog.Param("y", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("x"), Datalog.Var("y")))
          )),
          Datalog.Body(
            // TODO: Does that make sense ? Datalogs execution order is not fixed
            if (recursive == "left") {
              Seq(
                Datalog.Call("path", Seq(Datalog.Var("x"), Datalog.Var("z"))),
                Datalog.ExtensionalCall("edge", Seq(Datalog.Var("z"), Datalog.Var("y")))
              )
            } else {
              Seq(
                Datalog.ExtensionalCall("edge", Seq(Datalog.Var("x"), Datalog.Var("z"))),
                Datalog.Call("path", Seq(Datalog.Var("z"), Datalog.Var("y")))
              )
            }
          )
        )
      )
    ), Seq())
  }
}
