package inca.frontend.souffle.debugger

import inca.compiler.source.SourceString
import inca.debugger.Value
import inca.debugger.table.Table
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.lowering.{SouffleInputToEditscript, SouffleToDatalogIR}
import inca.frontend.souffle.parser.Parser
import inca.runtime.EnginePool
import inca.runtime.context.{DataModel, QueryScope}
import inca.runtime.db.Database
import org.eclipse.viatra.query.runtime.api.AdvancedViatraQueryEngine
import org.eclipse.viatra.query.runtime.rete.matcher.TimelyReteBackendFactory
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite
import truechange.{Edit, EditScript}

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

  val directsuperclassSig: Syntax.RuleSignature =
    Syntax.RuleSignature(Name("DirectSuperclass"), Seq(
      Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
      Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))), false)

  val pathProg: String =
    s"""
       |.decl edge(x: number, y: number)
       |.input edge(IO="file", filename="edge.facts", delimiter=";")
       |
       |.decl path(x: number, y: number)
       |path(X, Y) :- edge(X, Y).
       |path(X, Y) :- edge(X, Z), path(Z, Y).
       |""".stripMargin

  lazy val compiledModule: CompiledSouffleModule = compileSouffle(subclassTransitiveClosure)
  lazy val (engine, database): DatabaseRuntime = loadIncARuntime(compiledModule.dataModel)

  def compileSouffle(code: String): CompiledSouffleModule = {
    val ast = Parser.parse(SourceString(code))
    val compiler = new SouffleToDatalogIR
    compiler.compile("soufflemod", ast)
  }


  type DatabaseRuntime = (AdvancedViatraQueryEngine, Database)
  def loadIncARuntime(dataModel: DataModel): DatabaseRuntime = {
    val scope = new QueryScope(dataModel)
    EnginePool.loadEngineAndDatabase(scope, TimelyReteBackendFactory.FIRST_ONLY_SEQUENTIAL)
  }

  def loadInputs(rt: DatabaseRuntime, inputs: Map[Syntax.RuleSignature, String], delimiter: String = ";"): Unit = {
    val inputCompiler = new SouffleInputToEditscript("EMPTY")
    var edits: Seq[Edit] = Seq()
    inputs.foreach { case (sig, content) =>
      val rows = content.split("\n")
      val es = inputCompiler.compile(rows.toIterator, sig, delimiter)
      edits ++= es.edits
    }

    rt._1.delayUpdatePropagation(() =>
      rt._2.processEditScript(EditScript(edits))
    )
  }

  def initDebugger(prog: String, inputs: Map[Syntax.RuleSignature, String]): SouffleDebugger = {
    val compiled = compileSouffle(prog)
    val rt = loadIncARuntime(compiled.dataModel)
    loadInputs(rt, inputs)

    val debugger = new SouffleDebugger(compiled)
    debugger.setDatabaseRuntime(rt)
    debugger
  }

  def assertExpectedResult(rel: String, args: Table[Value], debugger: SouffleDebugger): Assertion = {
    val derived = debugger.relation(rel, args)
    val bottomUp = debugger.readDatabase(rel, args)
    assertResult(bottomUp)(derived)
  }

  test("one step transitive closure") {
    val superclasses =
      """A;B
        |B;C""".stripMargin

    val debugger = initDebugger(subclassTransitiveClosure, Map(directsuperclassSig -> superclasses))
    debugger.entry("Superclass", Table.unit)
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    assertExpectedResult("Superclass", Table.unit, debugger)
  }

  test("simple edge program") {
    val edges =
      """1;2
        |2;3
        |3;4
        |4;2""".stripMargin
    val edgeSig = Syntax.RuleSignature(Syntax.Name("edge"), Seq(
      Syntax.RuleParameter(Syntax.Name("x"), Syntax.NumberType),
      Syntax.RuleParameter(Syntax.Name("y"), Syntax.NumberType)), false)

    val debugger = initDebugger(pathProg, Map(edgeSig -> edges))
    debugger.entry("path", Table.unit)
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.stepInto()
    }
    assertExpectedResult("path", Table.unit, debugger)
  }
}
