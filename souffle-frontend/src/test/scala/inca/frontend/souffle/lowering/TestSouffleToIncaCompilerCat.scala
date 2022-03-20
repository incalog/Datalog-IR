package inca.frontend.souffle.lowering

import inca.compiler.source.SourceString
import inca.compiler.Options
import inca.frontend.souffle.parser.Parser
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.Name
import inca.runtime.context.DataModel
import inca.runtime.context.QueryScope
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

class TestSoufleToIncaCompilerCat extends AnyFlatSpec with IncaGPMatchers {

  val catProgram: String =
    """
      |.type Type
      |.type MethodDescriptor
      |.type Method
      |
      |.decl Method_DeclaringType(?method:Method, ?declaringType:ReferenceType)
      |.decl Method_ReturnType(?method:Method, ?returnType:Type)
      |.decl Method_SimpleName(?method:Method, ?simpleName:symbol)
      |.decl Method_ParamTypes(?method:Method, ?params:symbol)
      |.decl Method_Descriptor(?method:Method, ?descriptor:MethodDescriptor)
      |.decl _Method(?method:symbol, ?simplename:symbol, ?descriptor:symbol, ?declaringType:symbol, ?returnType:symbol, ?jvmDescriptor:symbol, ?arity:number)
      |.input _Method(IO="file", filename="Method.facts", delimiter="\t")
      |
      |Method_SimpleName(?method, ?simplename),
      |Method_ParamTypes(?method, ?params),
      |Method_DeclaringType(?method, ?declaringType),
      |Method_ReturnType(?method, ?returnType) :-
      |  _Method(?method, ?simplename, ?params, ?declaringType, ?returnType, ?jvmDescriptor, ?arity).
      |Method_Descriptor(?method, ?descriptor) :-
      |  Method_ReturnType(?method, ?returnType),
      |  Method_ParamTypes(?method, ?params),
      |  ?descriptor = cat(?returnType, cat("(", cat(?params, ")"))).
      |""".stripMargin

  lazy val compiledModule = {
    val ast = Parser.parse(SourceString(catProgram))
    val compiler = new SouffleToDatalogIR
    compiler.compile("catanalysis", ast)
  }

  val dataModel: DataModel = compiledModule.dataModel
  val scope: QueryScope = new QueryScope(dataModel)
  val options: Options = compiledModule.options

  val _MethodSig = Syntax.RuleSignature(
    Name("_Method"),
    Seq(
      Syntax.RuleParameter(Name("?method"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?simplename"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?descriptor"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?declaringType"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?returnType"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?jvmDescriptor"), Syntax.SymbolType),
      Syntax.RuleParameter(Name("?arity"), Syntax.NumberType)
    ),
    false
  )

  "compiled souffle" should "derive method descriptor correctly" in {
    val superclasses =
      "<sun.security.provider.MD4: int FF(int,int,int,int,int,int)>;FF;int,int,int,int,int,int;sun.security.provider.MD4;int;(IIIIII)I;6"
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val edit = factsCompiler.compile(superclasses.split("\n").iterator, _MethodSig, ";")

    assertMatch(compiledModule.ir, "Method_Descriptor", edit) { matcher =>
      println(matcher.getAllMatches)
      assert(matcher.getAllMatches.size == 1)
    }
  }
}
