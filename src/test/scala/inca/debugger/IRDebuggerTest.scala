package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.Table
import inca.runtime.context.DataModel
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class IRDebuggerTest extends AnyFunSuite {
  val singleEdgeMod = Datalog.Module(
    "SingleEdge",
    Seq(),
    Seq(
      Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq((Datalog.Var("from"), Datalog.TScalaInt)), Datalog.TScalaInt, Scala(q"(x: Int) => x + 1")))))))),
    Seq())

  val twoEdgeMod = Datalog.Module(
    "SingleEdge",
    Seq(),
    Seq(
      Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3")))))))),
  Seq())

  val mod = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 5"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7")))))
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
    debugger.entry("two", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    debugger.stepOver()
    assert(debugger.controlTrace.size == 2)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isPatternEndPoint)
    assert(debugger.isFinished)
    // debugger.stepInto()
    val expectedTable = Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(6)),
    )
    assert(debugger.relation("two") == Table(Seq("from", "to"), expectedTable))
  }

  test("step into body and over body") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Table.empty)
    debugger.stepInto() // into two
    debugger.stepOver() // over body
    debugger.stepOver() // out of two
    assert(debugger.controlTrace.size == 4)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isBodyPoint)
    assert(debugger.controlTrace(2).isBodyEndPoint)
    assert(debugger.controlTrace(3).isPatternEndPoint)
    assert(debugger.isFinished)
  }

  test("step into rule and into body") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Table.empty)
    debugger.stepInto() // into two
    debugger.stepInto() // into body
    debugger.stepOver() // over edge before
    debugger.stepOver() // over edge after
    debugger.stepOver() // over one before
    debugger.stepOver() // over one after
    debugger.stepOver() // out of body
    assert(debugger.controlTrace.size == 8)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isBodyPoint)
    assert(debugger.controlTrace(2).isAtomPoint)
    assert(debugger.controlTrace(3).isAtomEndPoint)
    assert(debugger.controlTrace(4).isAtomPoint)
    assert(debugger.controlTrace(5).isAtomEndPoint)
    assert(debugger.controlTrace(6).isBodyEndPoint)
    assert(debugger.controlTrace(7).isPatternEndPoint)
    assert(debugger.isFinished)
  }

  test("step into non-call atom") {
    val debugger = initDebugger(mod)
    debugger.entry("two", Table.empty)
    debugger.stepInto() // into two
    debugger.stepInto() // into body
    debugger.stepInto() // into edge call
    debugger.stepInto() // into edge pattern
    debugger.stepInto() // into body
    debugger.stepInto() // over first computed before
    debugger.stepInto() // over first computed after
    debugger.stepInto() // over second computed before
    debugger.stepOut() // out of edge body
    debugger.stepOut() // out of edge
    debugger.stepOver() // out of two body
    debugger.stepOut() // out of body
    debugger.stepOut() // out of two
    assert(debugger.controlTrace.size == 14)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isBodyPoint)
    assert(debugger.controlTrace(2).isAtomPoint)
    assert(debugger.controlTrace(3).isPatternPoint)
    assert(debugger.controlTrace(4).isBodyPoint)
    assert(debugger.controlTrace(5).isAtomPoint)
    assert(debugger.controlTrace(6).isAtomEndPoint)
    assert(debugger.controlTrace(7).isAtomPoint)
    assert(debugger.controlTrace(8).isAtomEndPoint)
    assert(debugger.controlTrace(9).isBodyEndPoint)
    assert(debugger.controlTrace(10).isPatternEndPoint)
    assert(debugger.controlTrace(11).isAtomEndPoint)
    assert(debugger.controlTrace(12).isBodyEndPoint)
    assert(debugger.controlTrace(13).isPatternEndPoint)
    assert(debugger.isFinished)
  }

  test("step into rule and into body then out of body") {
    val debugger = initDebugger(mod)
    debugger.entry("edge", Table.empty)
    debugger.stepInto() // into edge
    debugger.stepInto() // into body
    debugger.stepOut() // out of body
    debugger.stepOut() // out of pattern
    assert(debugger.controlTrace.size == 5)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isBodyPoint)
    assert(debugger.controlTrace(2).isAtomPoint)
    assert(debugger.controlTrace(3).isBodyEndPoint)
    assert(debugger.controlTrace(4).isPatternEndPoint)
    assert(debugger.isFinished)
  }

  // test("only step into") {
  //   val debugger = initDebugger(mod)
  //   debugger.entry("two", Table(Vector("from"), Vector(Vector(ScalaValue(1)))))
  //   debugger.stepInto() // into two
  //   debugger.stepInto() // over body
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   debugger.stepInto() // out of two
  //   assert(debugger.isFinished)
  // }

  test("two edge rule") {
    val debugger = initDebugger(twoEdgeMod)
    debugger.entry("edge", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    debugger.stepInto() // into edge
    debugger.stepInto() // into body
    debugger.stepInto() // execute computed before
    debugger.stepInto() // skip to seconded computed before
    debugger.stepInto() // execute seconded computed before
    debugger.stepInto() // get to body end
    debugger.stepInto() // get into second body
    debugger.stepInto() // execute computed before
    debugger.stepInto() // skip to seconded computed before
    debugger.stepInto() // execute seconded computed before
    debugger.stepInto() // get to body end
    debugger.stepInto() // get to pattern end
    debugger.stepInto() // get to pattern end
    assert(debugger.controlTrace.size == 14)
    assert(debugger.controlTrace(0).isPatternPoint)
    assert(debugger.controlTrace(1).isBodyPoint)
    assert(debugger.controlTrace(2).isAtomPoint)
    assert(debugger.controlTrace(3).isAtomEndPoint)
    assert(debugger.controlTrace(4).isAtomPoint)
    assert(debugger.controlTrace(5).isAtomEndPoint)
    assert(debugger.controlTrace(6).isBodyEndPoint)
    assert(debugger.controlTrace(7).isBodyPoint)
    assert(debugger.controlTrace(8).isAtomPoint)
    assert(debugger.controlTrace(9).isAtomEndPoint)
    assert(debugger.controlTrace(10).isAtomPoint)
    assert(debugger.controlTrace(11).isAtomEndPoint)
    assert(debugger.controlTrace(12).isBodyEndPoint)
    assert(debugger.controlTrace(13).isPatternEndPoint)
    assert(debugger.isFinished)
  }
}
