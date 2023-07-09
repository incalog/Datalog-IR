package inca.frontend.objectoriented.measurements

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.optimize.Optimization
import inca.backend.transform.Transformation
import inca.backend.transform.magic.demand.{DemandTransformation, DeriveDemandPatterns}
import inca.compiler.{CompiledModule, Options, SourceLocation}
import inca.runtime.context.DataModel
import inca.util.Scala

import scala.meta.XtensionQuasiquoteTerm

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

  /*def cycles(current: Int, step: Int, endNode: Int): Set[Seq[Int]] = {
    if (current < endNode)
      loop(current, current + step) ++ cycles(current + step, step, endNode)
    else
      line(current, endNode) ++ Set(Seq(endNode, current))
  }*/

  def fullyConnect(current: Int, startNode: Int, endNode: Int): Set[Seq[Int]] = {
    if (endNode > startNode)
      Set(Seq(current, endNode)) ++ fullyConnect(current, startNode, endNode - 1)
    else
      Set(Seq(current, endNode))
  }

  def cycles(current: Int, step: Int, startNode: Int, endNode: Int): Set[Seq[Int]] = {
    if (current < endNode)
      this.cycles(current + step, step, startNode, endNode) ++ fullyConnect(current, startNode, endNode)
    else if (current == endNode)
      this.line(startNode, endNode) ++ fullyConnect(current, startNode, endNode)
    else
      this.line(startNode, endNode)
  }

  def input(endNode: Int): Seq[Seq[Int]] = (line(1, 10) ++ loop(10, endNode)).toSeq

  def inputWithCycle(cycleStep: Int, endNode: Int): Seq[Seq[Int]] = cycles(1, cycleStep, 1, endNode).toSeq

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

  def moduleWithInputComputation(recursive: String): CompiledModule = new CompiledModule {
    override val options: Options = new Options {
      override def optimizations: Seq[Optimization] = Seq()
      override def transformations: Seq[Transformation] = Seq(DeriveDemandPatterns, DemandTransformation)
      override def stopOnError: Boolean = true
      override def stopOnWarning: Boolean = true
      override def withOptimizations(opts: Seq[Optimization]): Options = throw new RuntimeException("No optimizations")
      override def withTransformations(trans: Seq[Transformation]): Options = throw new RuntimeException("No transformations")
    }

    override def name: Datalog.Name = "Path"
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    override def dataModel: DataModel = new DataModel()

    override def ir: Datalog.Module = Datalog.Module("Path", Seq(), Seq(
      Datalog.Pattern(None, "line",
        Seq(
          Datalog.Param("from", Datalog.TScalaInt),
          Datalog.Param("to", Datalog.TScalaInt),
          Datalog.Param("out$1", Datalog.TScalaInt),
          Datalog.Param("out$2", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Computed(Datalog.True, Datalog.Evaluation(
                Seq(Datalog.Var("from") -> Datalog.TScalaInt, Datalog.Var("to") -> Datalog.TScalaInt),
                Datalog.TScalaBoolean,
                Scala(q"(f: Int, t: Int) =>  f < (t-1)")
              )),
              Datalog.Computed(Datalog.Var("out$2"), Datalog.Evaluation(
                Seq(Datalog.Var("from") -> Datalog.TScalaInt),
                Datalog.TScalaInt,
                Scala(q"(i: Int) => i + 1")
              )),
              Datalog.Eq(Datalog.Var("out$1"), Datalog.Var("from")),
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Computed(Datalog.True, Datalog.Evaluation(
                Seq(Datalog.Var("from") -> Datalog.TScalaInt, Datalog.Var("to") -> Datalog.TScalaInt),
                Datalog.TScalaBoolean,
                Scala(q"(f: Int, t: Int) =>  f < (t-1)")
              )),
              Datalog.Computed(Datalog.Var("from$1"), Datalog.Evaluation(
                Seq(Datalog.Var("from") -> Datalog.TScalaInt),
                Datalog.TScalaInt,
                Scala(q"(i: Int) => i + 1")
              )),
              Datalog.Call("line", Seq(Datalog.Var("from$1"), Datalog.Var("to"), Datalog.Var("out$1"), Datalog.Var("out$2")))
            )
          ),
          Datalog.Body(
            Seq(
            Datalog.Computed(Datalog.False, Datalog.Evaluation(
              Seq(Datalog.Var("from") -> Datalog.TScalaInt, Datalog.Var("to") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(f: Int, t: Int) =>  f < (t-1)")
            )),
            Datalog.Eq(Datalog.Var("out$1"), Datalog.Var("from")),
            Datalog.Eq(Datalog.Var("out$2"), Datalog.Var("to")),
          )
          )
        )
      ),
      Datalog.Pattern(None, "loop",
        Seq(
          Datalog.Param("from", Datalog.TScalaInt),
          Datalog.Param("to", Datalog.TScalaInt),
          Datalog.Param("out$1", Datalog.TScalaInt),
          Datalog.Param("out$2", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("line", Seq(Datalog.Var("from"), Datalog.Var("to"), Datalog.Var("out$1"), Datalog.Var("out$2")))
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Eq(Datalog.Var("out$1"), Datalog.Var("to")),
              Datalog.Eq(Datalog.Var("out$2"), Datalog.Var("from"))
            )
          )
        )
      ),
      Datalog.Pattern(None, "edge",
        Seq(
          Datalog.Param("endNode", Datalog.TScalaInt),
          Datalog.Param("x", Datalog.TScalaInt),
          Datalog.Param("y", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("line", Seq(Datalog.IntConstant(1), Datalog.IntConstant(10), Datalog.Var("x"), Datalog.Var("y"))),
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Call("loop", Seq(Datalog.IntConstant(10), Datalog.Var("endNode"), Datalog.Var("x"), Datalog.Var("y"))),
            )
          )
        )
      ),
      Datalog.Pattern(None, "path",
        Seq(
          Datalog.Param("endNode", Datalog.TScalaInt),
          Datalog.Param("x", Datalog.TScalaInt),
          Datalog.Param("y", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(Seq(
            Datalog.Call("edge", Seq(Datalog.Var("endNode"), Datalog.Var("x"), Datalog.Var("y")))
          )),
          Datalog.Body(
            // TODO: Does that make sense ? Datalogs execution order is not fixed
            if (recursive == "left") {
              Seq(
                Datalog.Call("path", Seq(Datalog.Var("endNode"), Datalog.Var("x"), Datalog.Var("z"))),
                Datalog.Call("edge", Seq(Datalog.Var("endNode"), Datalog.Var("z"), Datalog.Var("y")))
              )
            } else {
              Seq(
                Datalog.Call("edge", Seq(Datalog.Var("endNode"), Datalog.Var("x"), Datalog.Var("z"))),
                Datalog.Call("path", Seq(Datalog.Var("endNode"), Datalog.Var("z"), Datalog.Var("y")))
              )
            }
          )
        )
      ),
      Datalog.Pattern(None, "main",
        Seq(
          Datalog.Param("e", Datalog.TScalaInt),
          Datalog.Param("x", Datalog.TScalaInt),
          Datalog.Param("y", Datalog.TScalaInt)
        ),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("path", Seq(Datalog.Var("e"), Datalog.Var("x"), Datalog.Var("y")))
            )
          )
        )
      ).addHint(MagicSetHints.Main(Seq(true, false, false)))
    ), Seq())
  }
}
