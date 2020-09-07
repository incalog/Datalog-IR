package inca.souffle

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.souffle.Syntax._

import scala.collection.mutable

class SouffleToIncaCompiler {

  val patFuns: mutable.Map[String, PatternFunction] = mutable.Map()

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

    case RuleSignature(name, parameters, _) =>
      val fun = PatternFunction(None, name, parameters.map(compile), Seq(), Seq())
      patFuns += (funPrefix + name) -> fun

    case RuleDefinition(heads, rulebody) =>
      for (RuleHead(name, args) <- heads) {
        val fun = patFuns.getOrElse(funPrefix + name, throw new IllegalArgumentException(s"Unknown relation ${funPrefix + name}"))
        // rename all occurrences of the fun params in the Souffle rule
        val subst = fun.params.map(p => p.name -> ("$$" + p.name)).toMap
        val stmts = rulebody.map(compile(_, subst, funPrefix))
        val headEqs = (fun.params zip args).map {
          case (Param(name, _), arg) => Assert(Eq(Var(name), compile(arg, subst)))
        }
        val funbody = Body(headEqs ++ stmts)
        patFuns += (funPrefix + name) -> PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies :+ funbody)
      }

    case TypeDeclaration(name, superType) =>


    case Input(rule, filename, delimiter) =>

  }

  def compile(param: RuleParameter): Param =
    Param(param.name, compile(param.typ))

  def compile(typ: Syntax.Type): TypeAnno = typ match {
    case DeclaredType(name) => TNode(name)
    case SymbolType => TString
    case NumberType => TInt
    case UnsignedType => TLong
    case FloatType => TDouble
  }

  def compile(stm: Syntax.Statement, subst: Map[String, String], funPrefix: String): Core.Statement = stm match {
    case Equality(left, not, right) if !not =>
      Assert(Eq(compile(left, subst), compile(right, subst)))
    case Equality(left, not, right) if not =>
      Assert(Neq(compile(left, subst), compile(right, subst)))
    case RuleApplication(negated, component, rule, arguments) =>
      val call = component match {
        case Some(c) => Call(s"${c}_$rule", arguments.map(compile(_, subst)))
        case None => Call(funPrefix + rule, arguments.map(compile(_, subst)))
      }
      if (negated)
        Assert(Undef(call))
      else
        Assert(Def(call))
  }

  def compile(exp: Syntax.Expression, subst: Map[String, String]): Core.Exp = exp match {
    case Variable(name) =>  Core.Var(name)
    case StringValue(value) => Core.Constant(StringLiteral(value))
    case NumberValue(value) => Core.Constant(IntLiteral(value))
    case Syntax.Any => Core.Wildcard
    case BuiltInFunctionCall(fun, arguments) => ???
  }
}
