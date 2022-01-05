package inca.debugger

import inca.backend.ir.Datalog
import inca.runtime.context.DataModel
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class IRDebuggerTest extends AnyFunSuite {
  val mod = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 5"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))),
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7")))))
        )),
      Datalog.Pattern(None, "one", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))))
          )
        )),
      Datalog.Pattern(None, "two", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("one", Seq(Datalog.Var("temp"), Datalog.Var("to"))))
          )
        )),
      Datalog.Pattern(None, "three", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("two", Seq(Datalog.Var("temp"), Datalog.Var("to"))))
          )
        )),
    ),
    Seq()
  )
  val dataModel = new DataModel()

  def initDebugger(module: Datalog.Module): IRDebugger = {
    val debugger = new IRDebugger {}
    debugger.initialize(module)
    debugger
  }

  test("simple step over") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Map())
    debugger.stepOver()
    assert(debugger.isFinished)
  }

  test("step into body and over body") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Map())
    debugger.stepInto() // into two
    debugger.stepOver() // over body
    debugger.stepOver() // out of two
    assert(debugger.isFinished)
  }

  test("step into rule and into body") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Map())
    debugger.stepInto() // into two
    debugger.stepInto() // into body
    debugger.stepOver() // over edge
    debugger.stepOver() // over one
    debugger.stepOver() // out of body
    assert(debugger.isFinished)
  }

  test("step into rule and into body then out of body") {
    val debugger = initDebugger(mod)
    debugger.entry("edge", Map())
    debugger.stepInto() // into edge
    debugger.stepInto() // into body
    debugger.stepOut() // into body
    debugger.stepOut() // into body
    assert(debugger.isFinished)
  }
}
