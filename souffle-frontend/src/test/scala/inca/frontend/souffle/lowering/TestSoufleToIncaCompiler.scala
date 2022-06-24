package inca.frontend.souffle.lowering

import inca.compiler.source.SourceString
import inca.compiler.Options
<<<<<<< HEAD
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import inca.runtime.context.DataModel
=======
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
>>>>>>> debugger
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

<<<<<<< HEAD
  val directsuperclassSig: Syntax.RuleSignature =
=======
  val directsuperclassSig =
>>>>>>> debugger
    Syntax.RuleSignature(
      Name("DirectSuperclass"),
      Seq(
        Syntax.RuleParameter(Name("?class"), Syntax.DeclaredType(Name("ClassType"))),
        Syntax.RuleParameter(Name("?superclass"), Syntax.DeclaredType(Name("ClassType")))
      ),
<<<<<<< HEAD
      output = false
=======
      false
>>>>>>> debugger
    )

  lazy val compiledModule: CompiledSouffleModule = {
    val ast = Parser.parse(SourceString(subclassTransitiveClosure))
    val compiler = new SouffleToDatalogIR
    compiler.compile("transitiveclosure", ast)
  }

  val dataModel: DataModel = compiledModule.dataModel
  val scope: QueryScope = new QueryScope(dataModel)
  val options: Options = compiledModule.options

  "compiled souffle" should "trivial transitive closure" in {
    val superclasses =
      """A B
        |E G""".stripMargin
<<<<<<< HEAD
    val factsCompiler = new SouffleToNamedRelations("EMPTY")
    val inputs =
=======
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val directsuperclassEdits =
>>>>>>> debugger
      factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    println(compiledModule.psystemModule.patterns.keys)
    println(compiledModule.ir)
    println("OPT")
    println(compiledModule.optimized)
    assertMatch(compiledModule.ir, "Superclass", inputs) { matcher =>
      assert(matcher.getAllMatches.size == 2)
    }
  }

  "compiled souffle" should "one step transitive closure" in {
    val superclasses =
      """A B
        |B C""".stripMargin
<<<<<<< HEAD
    val factsCompiler = new SouffleToNamedRelations("EMPTY")
=======
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
>>>>>>> debugger
    val directsuperclassEdits =
      factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 3)
    }
  }

  "compiled souffle" should "two step transitive closure" in {
    val superclasses =
      """A B
        |B C
        |C D""".stripMargin
<<<<<<< HEAD
    val factsCompiler = new SouffleToNamedRelations("EMPTY")
=======
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
>>>>>>> debugger
    val directsuperclassEdits =
      factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

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
<<<<<<< HEAD
    val factsCompiler = new SouffleToNamedRelations("EMPTY")
=======
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
>>>>>>> debugger
    val directsuperclassEdits =
      factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

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
<<<<<<< HEAD
    val factsCompiler = new SouffleToNamedRelations("EMPTY")
=======
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
>>>>>>> debugger
    val directsuperclassEdits =
      factsCompiler.compile(superclasses.split("\n").iterator, directsuperclassSig, " ")

    assertMatch(compiledModule.ir, "Superclass", directsuperclassEdits) { matcher =>
      assert(matcher.getAllMatches.size == 15)
    }
  }
}
