package inca.debugger

import inca.analyzedLangs.{Exp, ExpLangTestAnalyses}
import inca.backend.ir.Datalog
import inca.compiler.{CompiledDatalogModule, Compiler, Options}
import inca.debugger.table.Table
import inca.examples.functional.Code
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.context.DataModel
import inca.util.Scala
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class IRDebuggerTest extends AnyFunSuite {
  def edgePattern(edges: (Int, Int)*): Datalog.Pattern =
    Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      edges.map { case (from, to) =>
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => $from"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => $to")))))
      })

  val singleEdgePattern =
    Datalog.Pattern(None, "edge", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Computed(Datalog.Var("from"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))),
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq((Datalog.Var("from"), Datalog.TScalaInt)), Datalog.TScalaInt, Scala(q"(x: Int) => x + 1")))))))
  val twoEdgePattern = edgePattern(1 -> 2, 1 -> 3)
  val sevenEdgePattern = edgePattern(1 -> 2, 1 -> 4, 1 -> 5, 2 -> 3, 2 -> 6, 4 -> 6, 6 -> 7)
  val simpleCycleEdgePattern = edgePattern(1 -> 2, 2 -> 1)
  val simpleCycleEdgePattern2 = edgePattern(1 -> 2, 2 -> 1, 2 -> 3)
  val cycleEdgePattern = edgePattern(1 -> 2, 1 -> 4, 1 -> 5, 2 -> 3, 2 -> 6, 4 -> 6, 6 -> 7, 3 -> 1)
  val threeHopCyclePattern = edgePattern(1 -> 2, 2 -> 3, 3 -> 1)

  val twoHopsModule = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      sevenEdgePattern,
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
    ),
    Seq()
  )
  val nodePattern =
    Datalog.Pattern(None, "node", Seq(Datalog.Param("n", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("n"), Datalog.Var("to"))))),
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("n")))))))

  val negationModule = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      nodePattern,
      sevenEdgePattern,
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
      Datalog.Pattern(None, "nodesNotTwoHop", Seq(Datalog.Param("x", Datalog.TScalaInt), Datalog.Param("y", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Call("node", Seq(Datalog.Var("x"))),
            Datalog.Call("node", Seq(Datalog.Var("y"))),
            Datalog.Call("two", Seq(Datalog.Var("x"), Datalog.Var("y")), neg = true)))
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
          Datalog.Computed(Datalog.Var("to"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))))),
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
          Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to"))))
      ))

  val countAggPattern =
    Datalog.Pattern(None, "numberOfEdges", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("res", Datalog.TScalaInt)),
      Seq(Datalog.Body(Seq(
        Datalog.Computed(Datalog.Var("res"), Datalog.CountAggregation("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))))
      ))))

  val countAggPatternCompareWithConst =
    Datalog.Pattern(None, "numberOfEdges", Seq(Datalog.Param("from", Datalog.TScalaInt)),
      Seq(Datalog.Body(Seq(
        Datalog.Computed(Datalog.Constant(Datalog.IntLiteral(3)), Datalog.CountAggregation("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))))
      ))))

  val pathPattern =
    Datalog.Pattern(None, "path", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))),
        )),
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
          Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to"))),
        ))))

  val pathPatternLeftRecursive =
    Datalog.Pattern(None, "path", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Call("path", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
          Datalog.Call("edge", Seq(Datalog.Var("temp"), Datalog.Var("to"))),
        )),
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))),
        )),
      ))

  val pathPatternSwitchBodies =
    Datalog.Pattern(None, "path", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("to", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
          Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to"))),
        )),
        Datalog.Body(Seq(
          Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to"))),
        ))))

  val notTargetOfPattern =
    Datalog.Pattern(None, "notTargetOf", Seq(Datalog.Param("from", Datalog.TScalaInt), Datalog.Param("n", Datalog.TScalaInt)),
      Seq(
        Datalog.Body(Seq(
          Datalog.Call("node", Seq(Datalog.Var("from"))),
          Datalog.Call("node", Seq(Datalog.Var("n"))),
          Datalog.Call("path", Seq(Datalog.Var("from"), Datalog.Var("n")), neg = true)))))

  def module(pats: Datalog.Pattern*): Datalog.Module =
    Datalog.Module("TestModule", Seq(), pats , Seq())

  val emptyDataModel = new DataModel()

  def stepTillFinish(debugger: IRDebugger): Unit = {
    while (!debugger.isFinished) {
      debugger.stepInto()
    }
  }


  def initDebugger(module: Datalog.Module, dataModel: DataModel): IRDebugger = {
    val compiled = CompiledDatalogModule(module, dataModel, Options(_stopOnError = true, _stopOnWarning = false, Seq(), Seq()))
    val debugger = new IRDebugger(compiled)
    debugger
  }

  def assertExpectedTable(debugger: IRDebugger, name: String, args: Table[Value]): Assertion = {
    val derived = debugger.relation(name, args)
    val expected = debugger.readDatabase(name, args)
    assertResult(expected)(derived)
  }

  // Step into tests
  test("simple stepinto ") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }


  test("test compare atoms") {
    val debugger = initDebugger(module(comparatorPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(4)), Seq(ScalaValue(1)), Seq(ScalaValue(2)), Seq(ScalaValue(3))))
    debugger.entry("edge", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "edge", args)
  }

  test("test has type atom") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val mulURI = tree.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

    val args = Table[Value](Seq("mul"), Seq(Seq(URIValue(mulURI))))
    debugger.entry("mul", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mul", args)
  }

  test("test two has type atoms join") {
    val tree = Exp.Mul(Exp.IntegerLit(1), Exp.IntegerLit(2))

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulIntLitPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

    val args = Table[Value](Seq("mul"), Seq(Seq(URIValue(tree.uri))))
    debugger.entry("mulIntLit", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "mulIntLit", args)
  }

  test("test path atom") {
    val tree = Exp.Mul(Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2)), Exp.IntegerLit(3))
    val mulURI = tree.uri
    val mulLhsURI = tree.lhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.lhsPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

    val args = Table[Value](Seq("exp"), Seq(Seq(URIValue(mulURI)), Seq(URIValue(mulLhsURI))))
    debugger.entry("lhs", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "lhs", args)
  }

  test("test path atom where trg is literal") {
    val tree = Exp.Add(Exp.IntegerLit(1), Exp.IntegerLit(2))
    val intLitLhs = tree.lhs.uri
    val intLitRhs = tree.rhs.uri

    val debugger = initDebugger(module(ExpLangTestAnalyses.intVal), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

    val args = Table[Value](Seq("exp"), Seq(Seq(URIValue(intLitLhs)), Seq(URIValue(intLitRhs))))
    debugger.entry("intVal", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "intVal", args)
  }

  test("test negative call") {
    val debugger = initDebugger(negationModule, emptyDataModel)
    val args = Table[Value](Seq("x"), Seq(Seq(ScalaValue(1))))
    debugger.entry("nodesNotTwoHop", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "nodesNotTwoHop", args)
  }

//  test("test count aggregation 1") {
//    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 2") {
//    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(7))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 3") {
//    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("test count aggregation 4") {
//    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
//    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(2))))
//    debugger.entry("numberOfEdges", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "numberOfEdges", args)
//  }
//
//  test("custom aggregation example") {
//    val compiledExample = Compiler.compileFunctional(Code.simpleFoldIntModule, FunctionalOptions())
//
//    val debugger = initDebugger(compiledExample.ir, new DataModel())
//    val args = Table[Value](Seq("start", "end"), Seq(Seq(ScalaValue(0), ScalaValue(4))))
//    debugger.entry("sum", args)
//    stepTillFinish(debugger)
//
//    assertExpectedTable(debugger, "sum", args)
//  }

  // recursive step into
  test("step into recursive pattern") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data") {
    val debugger = initDebugger(module(simpleCycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with simple cyclic data 2") {
    val debugger = initDebugger(module(simpleCycleEdgePattern2, pathPatternLeftRecursive), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into left recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPatternLeftRecursive), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern 2") {
    val debugger = initDebugger(module(sevenEdgePattern, pathPatternSwitchBodies), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with simple cyclic data") {
    val debugger = initDebugger(module(simpleCycleEdgePattern, pathPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }


  test("step into recursive pattern with simple cyclic data 2") {
    val debugger = initDebugger(module(simpleCycleEdgePattern2, pathPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with three hop cycle") {
    val debugger = initDebugger(module(threeHopCyclePattern, pathPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("step into recursive pattern with cyclic data") {
    val debugger = initDebugger(module(cycleEdgePattern, pathPattern), emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("path", args)
    stepTillFinish(debugger)

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "path", args)
  }

  test("test negative call of recursive pattern 1") {
    val debugger = initDebugger(module(sevenEdgePattern, nodePattern, pathPattern, notTargetOfPattern), emptyDataModel)
    val args = Table[Value](Seq("n"), Seq(Seq(ScalaValue(1))))
    debugger.entry("notTargetOf", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "notTargetOf", args)
  }

  test("test negative call of recursive pattern 2") {
    val debugger = initDebugger(module(sevenEdgePattern, nodePattern, pathPattern, notTargetOfPattern), emptyDataModel)
    val args = Table[Value](Seq("n"), Seq(Seq(ScalaValue(2))))
    debugger.entry("notTargetOf", args)
    stepTillFinish(debugger)

    assertExpectedTable(debugger, "notTargetOf", args)
  }

  // step over tests
  test("step over pattern") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepOver()
    debugger.stepOver()

    assertExpectedTable(debugger, "two", args)
  }

  test("step over body") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepInto()
    debugger.stepOver() // step over body

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern
    debugger.stepOver() // needed to step over pattern call again
    debugger.stepOver() // needed to pop last element from stack

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("step over call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    val args = Table[Value](Seq("from"), Seq(Seq(ScalaValue(1))))
    debugger.entry("two", args)
    debugger.stepInto()
    debugger.stepInto() // step into body
    debugger.stepOver() // step over edge call
    debugger.stepOver() // step over one call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to exit pattern
    debugger.stepOver() // needed to step over pattern call again
    debugger.stepOver() // needed to pop last element from stack

    assert(debugger.isFinished)
    assertExpectedTable(debugger, "two", args)
  }

  test("if example control") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample, FunctionalOptions())
    println(compiledExample.ir)
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      println(s"${debugger.controlPointIR}:\n  ${debugger.varsIR}")
      debugger.stepInto()
    }
    debugger.controlTraceIR.foreach(println)
    println(debugger.relation("main"))
  }

  test("if example 2 control") {
    val compiledExample = Compiler.compileFunctional(Code.ifExample2, FunctionalOptions())
    println(compiledExample.ir)
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("main", Table.unit)
    while (!debugger.isFinished) {
      println(s"${debugger.controlPointIR}:\n  ${debugger.varsIR}")
      debugger.stepInto()
    }
    debugger.controlTraceIR.foreach(println)
    println(debugger.relation("main"))
  }
}
