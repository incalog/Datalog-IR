package inca.frontend.souffle.debugger

import inca.compiler.Options
import inca.compiler.source.SourceString
import inca.debugger.table.Table
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import inca.frontend.souffle.lowering.{SouffleInputToEditscript, SouffleToDatalogIR}
import inca.frontend.souffle.parser.Parser
import inca.runtime.context.QueryScope
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

class SouffleDebuggerTest extends AnyFlatSpec with IncaGPMatchers {

  val subclassTransitiveClosure: String =
    """
      |.type Type
      |.type ReferenceType = Type
      |.type ClassType = ReferenceType
      |
      |.decl DirectSuperclass(?class:ClassType, ?superclass:ClassType)
      |.input DirectSuperclass(IO="file", filename="DirectSuperclass.facts", delimiter="\t")
      |
      |.decl DirectSubclass(?a:Type, ?c:Type)
      |.decl Subclass(?c:Type, ?a:Type)
      |.decl Superclass(?c:Type, ?a:Type)
      |
      |DirectSubclass(?a, ?c) :-
      |  DirectSuperclass(?a, ?c).
      |
      |Subclass(?c, ?a) :-
      |  DirectSubclass(?a, ?c).
      |Subclass(?c, ?a) :-
      |  Subclass(?b, ?a),
      |  DirectSubclass(?b, ?c).
      |.output Superclass
      |Superclass(?c, ?a) :-
      |  Subclass(?a, ?c).
      |
      |.printsize Superclass
      |""".stripMargin

  val directsuperclassSig =
    Syntax.RuleSignature(Name("DirectSuperclass"), Seq(
      Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
      Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))), false)

  lazy val compiledModule = {
    val ast = Parser.parse(SourceString(subclassTransitiveClosure))
    val compiler = new SouffleToDatalogIR
    compiler.compile("transitiveclosure", ast)
  }

  val dataModel = compiledModule.dataModel
  val scope: QueryScope = new QueryScope(dataModel)
  val options: Options = compiledModule.options

//  "compiled souffle" should "trivial transitive closure" in {
//    val superclasses =
//      """A B
//        |E G""".stripMargin
//    val factsCompiler = new SouffleInputToEditscript("EMPTY")
//    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")
//
//    println(compiledModule.psystemModule.patterns.keys)
//    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
//      assert(matcher.getAllMatches.size == 2)
//    }
//  }

  "souffle debugger" should "one step transitive closure" in {
    val superclasses =
      """A B
        |B C""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    println(compiledModule.ir)
    val debugger = new SouffleDebugger(compiledModule)
    debugger.entry("Superclass", directsuperclassEdits, Table.unit)
    while (!debugger.isFinished) {
      println(debugger.currentDebuggerInfo)
      debugger.souffleStepInto()
    }
    println(debugger.relation("Superclass"))
  }

//  "compiled souffle" should "two step transitive closure" in {
//    val superclasses =
//      """A B
//        |B C
//        |C D""".stripMargin
//    val factsCompiler = new SouffleInputToEditscript("EMPTY")
//    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")
//
//    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
//      assert(matcher.getAllMatches.size == 6)
//    }
//  }
//
//  "compiled souffle" should "three step transitive closure" in {
//    val superclasses =
//      """A B
//        |B C
//        |C D
//        |D E""".stripMargin
//    val factsCompiler = new SouffleInputToEditscript("EMPTY")
//    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")
//
//    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
//      assert(matcher.getAllMatches.size == 10)
//    }
//  }
//
//  "compiled souffle" should "four step transitive closure" in {
//    val superclasses =
//      """A B
//        |B C
//        |C D
//        |D E
//        |E F""".stripMargin
//    val factsCompiler = new SouffleInputToEditscript("EMPTY")
//    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")
//
//    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
//      assert(matcher.getAllMatches.size == 15)
//    }
//  }
}
