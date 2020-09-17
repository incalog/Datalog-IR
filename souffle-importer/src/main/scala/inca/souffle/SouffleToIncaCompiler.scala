package inca.souffle

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.souffle.Syntax._

import inca.souffle.Util._

import scala.collection.mutable

class SouffleToIncaCompiler {

  val patFuns: mutable.Map[String, PatternFunction] = mutable.Map()
  val decls: mutable.Map[String, RuleSignature] = mutable.Map()
  val inputs: mutable.Map[String, Input] = mutable.Map()

  val componentDefinitions: mutable.Map[String, ComponentDefinition] = mutable.Map()

  def compile(name: String, analysis: Analysis): Module = {
    analysis.contents.foreach(compile(_, ""))
    Module(name, Seq(), patFuns.values.toSeq.sortBy(_.name))
  }

  def compile(content: AnalysisContent, funPrefix: String): Unit = content match {
    case cdef@ComponentDefinition(name, contents) =>
      componentDefinitions += name -> cdef

    case ComponentInitialization(name, composite) =>
      val cdef = componentDefinitions.getOrElse(composite, throw new IllegalArgumentException(s"Unknown component definition $composite"))
      cdef.contents.foreach(compile(_, name + "_"))

    case s@RuleSignature(name, parameters, _) =>
      val fun = PatternFunction(None, name, parameters.map(compile), Seq(), Seq())
      patFuns += (funPrefix + name) -> fun
      decls += name -> s

    case RuleDefinition(heads, rulebody) =>
      for (RuleHead(name, args) <- heads) {
        val fun = patFuns.getOrElse(funPrefix + name, throw new IllegalArgumentException(s"Unknown relation ${funPrefix + name}"))
        val headEqs = (fun.params zip args).map {
          case (Param(name, _), arg) => Assert(Eq(Var(name), compile(arg)))
        }
        val stmts = rulebody.map(compile(_, funPrefix))
        val funbody = Body(headEqs ++ stmts)
        patFuns += (funPrefix + name) -> PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies :+ funbody)
      }

    case TypeDeclaration(name, superType) =>

    case in@Input(rule, filename, delimiter) =>
      val decl = decls(rule)
      inputs(rule) = in
      // generate pattern that enumerates all node instances of AST node class
      val fun = patFuns.getOrElse(rule, throw new IllegalArgumentException("Rule signature has to come before input declaration"))
      val body = Core.Body(
        Values("node", TNode(rule)) +:
        decl.parameters.map { param =>
          val strippedName = cleanSouffleName(param.name)
          Assert(Eq(PathAccess(Var("node"), NamedLink(TNode(rule), strippedName)), Var(strippedName)))
        }
      )
      patFuns(rule) = PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, Seq(body))
      println(patFuns(rule).prettyprint(""))

    case Output(rule) =>

    case PrintSize(rule) =>
  }

  def compile(param: RuleParameter): Param =
    Param(cleanSouffleName(param.name), compile(param.typ))

  def compile(typ: Syntax.Type): TypeAnno = typ match {
    case DeclaredType(name) => TString
    case SymbolType => TString
    case NumberType => TInt
    case UnsignedType => TLong
    case FloatType => TDouble
  }

  def compile(stm: Syntax.Statement, funPrefix: String): Core.Statement = stm match {
    case Equality(left, not, right) if !not =>
      Assert(Eq(compile(left), compile(right)))
    case Equality(left, not, right) if not =>
      Assert(Neq(compile(left), compile(right)))
    case RuleApplication(negated, component, rule, arguments) =>
      val call = component match {
        case Some(c) => Call(s"${c}_$rule", arguments.map(compile))
        case None => Call(funPrefix + rule, arguments.map(compile))
      }
      if (negated)
        Assert(Undef(call))
      else
        Assert(Def(call))
  }

  def compile(exp: Syntax.Expression): Core.Exp = exp match {
    case Variable(name) =>  Core.Var(cleanSouffleName(name))
    case StringValue(value) => Core.Constant(StringLiteral(value))
    case NumberValue(value) => Core.Constant(IntLiteral(value))
    case Syntax.Any => Core.Wildcard
    case BuiltInFunctionCall(CatBuiltInFunction, arguments) =>
      val params = collectParams(exp)
      Core.Eval(params, TString, compileEvalString(exp))
    case _ => throw new IllegalArgumentException(s"TODO $exp not supported")
  }

  def collectParams(exp: Syntax.Expression): Seq[String] = exp match {
    case Variable(name) => Seq(cleanSouffleName(name))
    case StringValue(value) => Seq()
    case NumberValue(value) => Seq()
    case BuiltInFunctionCall(fun, args) => args.flatMap(collectParams)
    case Syntax.Any => throw new IllegalArgumentException("Any is not supported in BuiltInFunctionCall")
  }

  def compileEvalString(exp: Syntax.Expression): String = exp match {
    case Variable(name) => cleanSouffleName(name)
    case StringValue(value) => "\"" + value + "\""
    case NumberValue(value) => value.toString
    case BuiltInFunctionCall(fun, args) =>
      val lhs = compileEvalString(args.head)
      val rhs = compileEvalString((args(1)))
      s"($lhs + $rhs)"
    case Syntax.Any => throw new IllegalArgumentException("Any is not supported in BuiltInFunctionCall")
  }
}
