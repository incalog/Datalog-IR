package inca.debugger

import inca.analyzedLangs.{Exp, ExpLangTestAnalyses}
import inca.backend.ir.Datalog
import inca.debugger.table.Table
import inca.runtime.context.DataModel
import inca.util.Meta.Scala
import org.scalatest.funsuite.AnyFunSuite
import truechange.EditScript
import truediff.Diffable

import scala.meta.XtensionQuasiquoteTerm

class IRDebuggerTest extends AnyFunSuite {
  val singleEdgePattern =
    Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq((Datalog.Var("from"), Datalog.TScalaInt)), Datalog.TScalaInt, Scala(q"(x: Int) => x + 1")))))))

  val twoEdgePattern =
      Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
            Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3")))))))

  val twoHopsModule = Datalog.Module(
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

  val comparatorPattern =
    Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
          Datalog.Compare(Datalog.EqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))),
          Datalog.Compare(Datalog.EqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))),
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Compare(Datalog.EqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("to"), Datalog.Var("from")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 5"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 5"))),
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to")))),
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))),
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("to"), Datalog.Var("from")))),
    ))

  def module(pat: Datalog.Pattern): Datalog.Module =
    Datalog.Module("TestModule", Seq(), Seq(pat), Seq())

  val emptyDataModel = new DataModel()

  def initDebugger(module: Datalog.Module, dataModel: DataModel, tree: Diffable): IRDebugger =
    initDebugger(module, dataModel, tree.loadEdits)

  def stepTillFinish(debugger: IRDebugger): Unit = {
    while (!debugger.isFinished)
      debugger.stepInto()
  }


  def initDebugger(module: Datalog.Module, dataModel: DataModel, edits: EditScript = EditScript(Seq())): IRDebugger = {
    val debugger = new IRDebugger {}
    debugger.initialize(module, dataModel, edits)
    debugger
  }

  test("simple step over") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    debugger.entry("two", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    stepTillFinish(debugger)

    assert(debugger.isFinished)

    val expectedTable = Table(Seq("from", "to"), Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(6)),
    ))
    val rel = debugger.relation("two")
    assertResult(expectedTable)(rel)
  }


  test("test compare atoms") {
    val debugger = initDebugger(module(comparatorPattern), emptyDataModel)
    debugger.entry("edge", Table(Seq("from"), Seq(Seq(ScalaValue(4)), Seq(ScalaValue(1)), Seq(ScalaValue(2)), Seq(ScalaValue(3)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("edge")

    val expectedTable = Table(Seq("from", "to"), Seq(
      Seq(ScalaValue(4), ScalaValue(4)),
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(1)),
      Seq(ScalaValue(2), ScalaValue(1)),
      Seq(ScalaValue(3), ScalaValue (1)),
    ))
    assertResult(expectedTable)(rel)
  }

  test("test has type atom") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val mulURI = tree.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulPattern), Exp.model, tree)
    debugger.entry("mul", Table(Seq("mul"), Seq(Seq(URIValue(mulURI)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("mul")

    val expectedTable = Table(Seq("mul"), Seq(
      Seq(URIValue(mulURI))
    ))

    assertResult(expectedTable)(rel)
  }

  test("test two has type atoms join") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val mulURI = tree.uri
    val intLit1URI = tree.lhs.uri
    val intLit2URI = tree.rhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulIntLitPattern), Exp.model, tree)
    debugger.entry("mulIntLit", Table(Seq("mul"), Seq(Seq(URIValue(tree.uri)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("mulIntLit")

    val expectedTable = Table(Seq("mul", "intLit"), Seq(
      Seq(URIValue(mulURI), URIValue(intLit1URI)),
      Seq(URIValue(mulURI), URIValue(intLit2URI))
    ))

    assertResult(expectedTable)(rel)
  }

  test("test path atom") {
    val tree = Exp.Mul(Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2)), Exp.IntegerLit(3))
    val mulURI = tree.uri
    val mulLhsURI = tree.lhs.uri
    val addLhsURI = tree.lhs.asInstanceOf[Exp.Add].lhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.lhsPattern), Exp.model, tree)
    debugger.entry("lhs", Table(Seq("exp"), Seq(Seq(URIValue(mulURI)), Seq(URIValue(mulLhsURI)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("lhs")

    val expectedTable = Table(Seq("exp", "res"), Seq(
      Seq(URIValue(mulURI), URIValue(mulLhsURI)),
      Seq(URIValue(mulLhsURI), URIValue(addLhsURI))
    ))

    assertResult(expectedTable)(rel)
  }

  test("test path atom where trg is literal") {
    val tree = Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val intLitLhs = tree.lhs.uri
    val intLitRhs = tree.rhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.intVal), Exp.model, tree)
    debugger.entry("intVal", Table(Seq("exp"), Seq(Seq(URIValue(intLitLhs)), Seq(URIValue(intLitRhs)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("intVal")

    val expectedTable = Table(Seq("exp", "v"), Seq(
      Seq(URIValue(intLitLhs), ScalaValue(1)),
      Seq(URIValue(intLitRhs), ScalaValue(2))
    ))
    assertResult(expectedTable)(rel)
  }
}
