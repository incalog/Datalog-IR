package inca.frontend.souffle.lowering

import inca.compiler.Options
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.parser.Parser
import inca.runtime.context.QueryScope
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

class TestSoufleToIncaCompiler extends AnyFlatSpec with IncaGPMatchers {

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
      |.output Superclass
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
      |""".stripMargin

  val directsuperclassSig =
    Syntax.RuleSignature("DirectSuperclass", Seq(
      Syntax.RuleParameter("?class", Syntax.DeclaredType("ClassType")),
      Syntax.RuleParameter("?superclass", Syntax.DeclaredType("ClassType"))), false)

  lazy val compiledModule = {
    val ast = Parser.parse(subclassTransitiveClosure)
    val compiler = new SouffleToIncaBackendCompiler
    compiler.compile("transitiveclosure", ast)
  }

  val dataModel = compiledModule.dataModel
  val scope: QueryScope = new QueryScope(dataModel)
  val options: Options = compiledModule.options

  "compiled souffle" should "trivial transitive closure" in {
    val superclasses =
      """A B
        |E G""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  "compiled souffle" should "one step transitive closure" in {
    val superclasses =
      """A B
        |B C""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  "compiled souffle" should "two step transitive closure" in {
    val superclasses =
      """A B
        |B C
        |C D""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 6)
    }
  }

  "compiled souffle" should "three step transitive closure" in {
    val superclasses =
      """A B
        |B C
        |C D
        |D E""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 10)
    }
  }

  "compiled souffle" should "four step transitive closure" in {
    val superclasses =
      """A B
        |B C
        |C D
        |D E
        |E F""".stripMargin
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits = factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 15)
    }
  }
}
