package inca.souffle

import inca.IncaMatchers
import inca.compiler.Options
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.scalatest.flatspec.AnyFlatSpec

class TestSoufleToIncaCompilerCat extends AnyFlatSpec with IncaMatchers {


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
    val ast = Parser(catProgram.linesIterator)
    val compiler = new SouffleToIncaCompiler
    compiler.compile("catanalysis", ast)
  }

  private val lang: LanguageMetaInfo = compiledModule.compilerOptions.languageMetaInfo
  val scope: QueryScope = new QueryScope(lang)
  val options: Options = compiledModule.compilerOptions


  val _MethodSig = Syntax.RuleSignature("_Method", Seq(
    Syntax.RuleParameter("?method", Syntax.SymbolType),
    Syntax.RuleParameter("?simplename", Syntax.SymbolType),
    Syntax.RuleParameter("?descriptor", Syntax.SymbolType),
    Syntax.RuleParameter("?declaringType", Syntax.SymbolType),
    Syntax.RuleParameter("?returnType", Syntax.SymbolType),
    Syntax.RuleParameter("?jvmDescriptor", Syntax.SymbolType),
    Syntax.RuleParameter("?arity", Syntax.NumberType)),
    false)

  "compiled souffle" should "derive method descripter correctly" in {
    val superclasses =
      "<sun.security.provider.MD4: int FF(int,int,int,int,int,int)>;FF;int,int,int,int,int,int;sun.security.provider.MD4;int;(IIIIII)I;6"
    val factsCompiler = new SouffleInputToEditscript("EMPTY")
    val edit = factsCompiler.compile(superclasses.split("\n").iterator, _MethodSig, ";")
    println(compiledModule.ir)
    assertMatchGPEdit(compiledModule.ir, "Method_Descriptor", edit) { matcher =>
      println(matcher.getAllMatches)
      assert(matcher.getAllMatches.size == 1)
    }
  }
}
