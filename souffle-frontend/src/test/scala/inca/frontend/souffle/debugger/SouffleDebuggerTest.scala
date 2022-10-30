package inca.frontend.souffle.debugger

import inca.backend.optimize.InlineSimpleRelations
import inca.compiler.source.{Source, SourceFile, SourceString}
import inca.debugger.{ScalaValue, Value}
import inca.debugger.table.ImmutableTable
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.compiler.SouffleOptions
import inca.frontend.souffle.executor.SouffleExecutor
import inca.frontend.souffle.lowering.SouffleToDatalogIR
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import inca.frontend.souffle.executor.SouffleExecutor.loadInputs

import java.nio.file.Path
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Assertion

class SouffleDebuggerTest extends AnyFunSuite {

  val subclassTransitiveClosure: String =
    """
      |.type Type
      |.type ReferenceType = Type
      |.type ClassType = ReferenceType
      |
      |.decl DirectSuperclass(?class:ClassType, ?superclass:ClassType)
      |.input DirectSuperclass(IO="file", filename="DirectSuperclass.facts", delimiter="\t")
      |
      |.decl DirectSubclass(?c:Type, ?a:Type)
      |.decl Subclass(?c:Type, ?a:Type)
      |.decl Superclass(?a:Type, ?c:Type)
      |
      |DirectSubclass(?c, ?a) :-
      |  DirectSuperclass(?a, ?c).
      |
      |Subclass(?c, ?a) :-
      |  DirectSubclass(?c, ?a).
      |Subclass(?c, ?a) :-
      |  Subclass(?b, ?a),
      |  DirectSubclass(?c, ?b).
      |.output Superclass
      |Superclass(?a, ?c) :-
      |  Subclass(?c, ?a).
      |
      |.printsize Superclass
      |""".stripMargin

  val pathProg: String =
    s"""
      |.decl edge(x: number, y: number)
      |.input edge(IO="file", filename="edge.facts", delimiter=";")
      |
      |.decl path(x: number, y: number)
      |path(X, Y) :- edge(X, Y).
      |path(X, Y) :- edge(X, Z), path(Z, Y).
      |""".stripMargin

  def compileSouffle(code: Source): CompiledSouffleModule = {
    val ast = Parser.parse(code)
    val compiler = new SouffleToDatalogIR
    compiler.compile("soufflemod", ast)
  }

  def initDebugger(
      prog: Source,
      inputs: Map[Syntax.RuleSignature, String],
      options: SouffleOptions = SouffleOptions()
    ): SouffleDebugger = {
    val noInlineOptions = options.withOptimizations(options.optimizations.filter(_ != InlineSimpleRelations))
    val module = SouffleExecutor.compileSouffle(prog, noInlineOptions)
    val input = SouffleExecutor.loadInputs(inputs, module, ";")
    val debugger = new SouffleDebugger(module, input)
    debugger
  }

  def assertExpectedResult(
      rel: String,
      args: ImmutableTable[Value],
      debugger: SouffleDebugger
    ): Assertion = {
    val derived = debugger.callStack.top.predResult
    val bottomUp = debugger.state.readBottomUp(rel, args)
    assertResult(bottomUp)(derived)
  }

  test("one step transitive closure") {
    val superclasses =
      """A;B
        |B;C""".stripMargin

    val directsuperclassSig: Syntax.RuleSignature =
      Syntax.RuleSignature(
        Name("DirectSuperclass"),
        Seq(
          Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
          Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))
        ),
        output = false
      )

    val debugger = initDebugger(
      SourceString(subclassTransitiveClosure),
      Map(directsuperclassSig -> superclasses)
    )
    debugger.entry("Superclass", ImmutableTable.unit[Value]())

    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertExpectedResult("Superclass", ImmutableTable.unit[Value](), debugger)
  }

  test("one step transitive closure from A") {
    val superclasses =
      """A;B
        |B;C""".stripMargin

    val directsuperclassSig: Syntax.RuleSignature =
      Syntax.RuleSignature(
        Name("DirectSuperclass"),
        Seq(
          Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
          Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))
        ),
        output = false
      )

    val debugger = initDebugger(
      SourceString(subclassTransitiveClosure),
      Map(directsuperclassSig -> superclasses)
    )
    val debuggerInput = ImmutableTable[Value](Seq("a"), Seq(Seq(ScalaValue("A"))))
    debugger.entry("Superclass", debuggerInput)

    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertExpectedResult("Superclass", debuggerInput, debugger)
  }

  test("two step transitive closure") {
    val superclasses =
      """A;B
        |B;C
        |C;D""".stripMargin

    val directsuperclassSig: Syntax.RuleSignature =
      Syntax.RuleSignature(
        Name("DirectSuperclass"),
        Seq(
          Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
          Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))
        ),
        output = false
      )

    val debugger = initDebugger(
      SourceString(subclassTransitiveClosure),
      Map(directsuperclassSig -> superclasses)
    )
    val debuggerInput = ImmutableTable[Value](Seq("a"), Seq(Seq(ScalaValue("A"))))
    debugger.entry("Superclass", debuggerInput)

    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertExpectedResult("Superclass", debuggerInput, debugger)
  }

  test("two step transitive closure from A") {
    val superclasses =
      """A;B
        |B;C
        |C;D""".stripMargin

    val directsuperclassSig: Syntax.RuleSignature =
      Syntax.RuleSignature(
        Name("DirectSuperclass"),
        Seq(
          Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
          Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))
        ),
        output = false
      )

    val debugger = initDebugger(
      SourceString(subclassTransitiveClosure),
      Map(directsuperclassSig -> superclasses)
    )
    debugger.entry("Superclass", ImmutableTable.unit[Value]())

    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertExpectedResult("Superclass", ImmutableTable.unit[Value](), debugger)
  }

  test("simple edge program") {
    val edges =
      """1;2
        |2;3
        |3;4
        |4;2""".stripMargin
    val edgeSig = Syntax.RuleSignature(
      Syntax.Name("edge"),
      Seq(
        Syntax.RuleParameter(Syntax.Name("x"), Syntax.NumberType),
        Syntax.RuleParameter(Syntax.Name("y"), Syntax.NumberType)
      ),
      output = false
    )

    val debugger = initDebugger(SourceString(pathProg), Map(edgeSig -> edges))
    debugger.entry("path", ImmutableTable.unit[Value]())
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo())
      debugger.stepInto()
    }
    assertExpectedResult("path", ImmutableTable.unit[Value](), debugger)
  }

  def pointsToDebugger: SouffleDebugger = {
    val benchmarkPath = "souffle-frontend/benchmark"
    val file = Path.of(s"$benchmarkPath/self-contained.dl")
    val factsDir = s"$benchmarkPath/minijavac"
    val options = SouffleOptions(mode = DRedReteBackendFactory.INSTANCE)
    val noInlineOptions = options.withOptimizations(options.optimizations.filter(_ != InlineSimpleRelations))
    val module = SouffleExecutor.compileSouffle(SourceFile(file), noInlineOptions)
    val input = SouffleExecutor.loadInputs(factsDir, module)
    val debugger = new SouffleDebugger(module, input)
    debugger
  }


  ignore("var points to analysis") {
    val debugger = pointsToDebugger
    val args = ImmutableTable.unit[Value]()
    // val args = ImmutableTable[Value](Seq("?var"), Seq(Seq(ScalaValue("x"))))
    debugger.entry("VarPointsTo", args)
    debugger.resume()
    assert(debugger.isFinished)
    // println(debugger.currentDebuggerInfo)
  }
}
