package inca.debugger

import inca.analyzedLangs.{Exp, ExpLangTestAnalyses}
import inca.backend.ir.Datalog
import inca.compiler.{CompiledDatalogModule, Compiler, Options}
import inca.debugger.table.Table
import inca.examples.functional.Code
import inca.frontend.functional.compiler.FunctionalOptions
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.Scala
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import truechange.EditScript

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

  val sevenEdgePattern =
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
      ))

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

  val negationModule = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      Datalog.Pattern(None, "node", Seq(Datalog.Param("n", Datalog.TScalaInt)),
        Seq(
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 2"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 4"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 5"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 6"))))),
          Datalog.Body(Seq(
            Datalog.Computed(Datalog.Var("n"), Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 7"))))),
        )),
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

  def module(pats: Datalog.Pattern*): Datalog.Module =
    Datalog.Module("TestModule", Seq(), pats , Seq())

  val emptyDataModel = new DataModel()

  def stepTillFinish(debugger: IRDebugger): Unit = {
    while (!debugger.isFinished)
      debugger.stepInto()
  }


  def initDebugger(module: Datalog.Module, dataModel: DataModel): IRDebugger = {
    val compiled = CompiledDatalogModule(module, dataModel, Options(_stopOnError = true, _stopOnWarning = false, Seq(), Seq()))
    val debugger = new IRDebugger(compiled)
    debugger
  }

  // Step into tests
  test("simple stepinto over") {
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

  test("execute prog") {
    val options = Options(_stopOnError = true, _stopOnWarning = false)
    val compiled = inca.compiler.Compiler.compileGP(twoHopsModule, emptyDataModel, options)
    val scope = new QueryScope(compiled.dataModel)
    val (engine, db) = EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
    engine.delayUpdatePropagation { () =>
      db.processEditScript(EditScript(Seq()))
    }
    val spec = compiled.psystemModule.patterns("two")
    val matcher = engine.getMatcher(spec())
    matcher.getAllMatchArrays.foreach { tuple =>
      println(tuple.toString)
    }

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

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

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

    val debugger = initDebugger(module(ExpLangTestAnalyses.mulIntLitPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

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

    val debugger = initDebugger(module(ExpLangTestAnalyses.lhsPattern), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

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

    val debugger = initDebugger(module(ExpLangTestAnalyses.intVal), Exp.model)
    debugger.updateExtensionalData(tree.loadEdits)

    debugger.entry("intVal", Table(Seq("exp"), Seq(Seq(URIValue(intLitLhs)), Seq(URIValue(intLitRhs)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("intVal")

    val expectedTable = Table(Seq("exp", "v"), Seq(
      Seq(URIValue(intLitLhs), ScalaValue(1)),
      Seq(URIValue(intLitRhs), ScalaValue(2))
    ))
    assertResult(expectedTable)(rel)
  }

  test("test negative call") {
    val debugger = initDebugger(negationModule, emptyDataModel)
    debugger.entry("nodesNotTwoHop", Table(Seq("x"), Seq(Seq(ScalaValue(1)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("nodesNotTwoHop")

    val expectedTable = Table(Seq("x", "y"), Seq(
      Seq(ScalaValue(1), ScalaValue(1)),
      Seq(ScalaValue(1), ScalaValue(2)),
      Seq(ScalaValue(1), ScalaValue(4)),
      Seq(ScalaValue(1), ScalaValue(5)),
      Seq(ScalaValue(1), ScalaValue(7)),
    ))
    assertResult(expectedTable)(rel)
  }

  test("test count aggregation 1") {
    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
    debugger.entry("numberOfEdges", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("numberOfEdges")

    val expectedTable = Table(Seq("from", "res"), Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
    ))
    assertResult(expectedTable)(rel)
  }

  test("test count aggregation 2") {
    val debugger = initDebugger(module(countAggPattern, sevenEdgePattern), emptyDataModel)
    debugger.entry("numberOfEdges", Table(Seq("from"), Seq(Seq(ScalaValue(7)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("numberOfEdges")

    val expectedTable = Table(Seq("from", "res"), Seq(
      Seq(ScalaValue(7), ScalaValue(0)),
    ))
    assertResult(expectedTable)(rel)
  }

  test("test count aggregation 3") {
    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
    debugger.entry("numberOfEdges", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("numberOfEdges")

    val expectedTable = Table(Seq("from"), Seq(
      Seq(ScalaValue(1)),
    ))
    assertResult(expectedTable)(rel)
  }

  test("test count aggregation 4") {
    val debugger = initDebugger(module(countAggPatternCompareWithConst, sevenEdgePattern), emptyDataModel)
    debugger.entry("numberOfEdges", Table(Seq("from"), Seq(Seq(ScalaValue(2)))))
    stepTillFinish(debugger)
    val rel = debugger.relation("numberOfEdges")

    val expectedTable = Table.empty(Seq("from"))
    assertResult(expectedTable)(rel)
  }

  test("custom aggregation example") {
    val compiledExample = Compiler.compileFunctional(Code.simpleFoldIntModule, FunctionalOptions())
    println(compiledExample.ir)
    val debugger = initDebugger(compiledExample.ir, new DataModel())
    debugger.entry("sum", Table(Seq("start", "end"), Seq(Seq(ScalaValue(0), ScalaValue(4)))))
    stepTillFinish(debugger)
    val res = debugger.relation("sum")

    val expected = Table(Seq("start", "end", "out$0"), Seq(Seq(ScalaValue(0), ScalaValue(4), ScalaValue(10))))
    assertResult(expected)(res)
  }


  // step over tests
  test("step over pattern") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    debugger.entry("two", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    debugger.stepOver()
    debugger.stepOver()

    assert(debugger.isFinished)
    val expectedTable = Table(Seq("from", "to"), Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(6)),
    ))
    val rel = debugger.relation("two")
    assertResult(expectedTable)(rel)
  }

  test("step over body") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    debugger.entry("two", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    debugger.stepInto()
    debugger.stepOver() // step over body

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to pop last element from stack

    assert(debugger.isFinished)
    val expectedTable = Table(Seq("from", "to"), Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(6)),
    ))
    val rel = debugger.relation("two")
    assertResult(expectedTable)(rel)
  }

  test("step over call") {
    val debugger = initDebugger(twoHopsModule, emptyDataModel)
    debugger.entry("two", Table(Seq("from"), Seq(Seq(ScalaValue(1)))))
    debugger.stepInto()
    debugger.stepInto() // step into body
    debugger.stepOver() // step over edge call
    debugger.stepOver() // step over one call

    debugger.stepOver() // needed to step to pattern exit
    debugger.stepOver() // needed to pop last element from stack

    assert(debugger.isFinished)
    val expectedTable = Table(Seq("from", "to"), Seq(
      Seq(ScalaValue(1), ScalaValue(3)),
      Seq(ScalaValue(1), ScalaValue(6)),
    ))
    val rel = debugger.relation("two")
    assertResult(expectedTable)(rel)
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
