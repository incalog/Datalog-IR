package inca.souffle

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.souffle.Syntax._

import scala.collection.mutable
import scala.io.Source
import scala.meta.{Defn, Lit, Term, Type}
import scala.meta.quasiquotes._

// Important: Legacy souffle code .type Type will translate to .type Type <: symbol
class SouffleToIncaCompiler {

  val patFuns: mutable.Map[String, PatternFunction] = mutable.Map()
  val decls: mutable.Map[String, RuleSignature] = mutable.Map()
  val types: mutable.ListBuffer[Defn.Type] = mutable.ListBuffer()
  val caseClasses: mutable.ListBuffer[Defn.Class] = mutable.ListBuffer()
  val objects: mutable.Map[String, mutable.ListBuffer[Term.Apply]] = mutable.Map()

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
        // rename all occurrences of the fun params in the Souffle rule
        val subst = fun.params.map(p => p.name -> ("$$" + p.name)).toMap
        val stmts = rulebody.map(compile(_, subst, funPrefix))
        val headEqs = (fun.params zip args).map {
          case (Param(name, _), arg) => Assert(Eq(Var(name), compile(arg, subst)))
        }
        val funbody = Body(headEqs ++ stmts)
        patFuns += (funPrefix + name) -> PatternFunction(fun.vis, fun.name, fun.params, fun.outParams, fun.bodies :+ funbody)
      }

    case TypeDeclaration(name, superType) => superType match {
      case None => types += q"type ${Type.Name(name)} = String"
      case Some(stype) => types += q"type ${Type.Name(name)} = ${genScalaType(stype)}"
    }


    case Input(rule, filename, delimiter) =>
      val decl = decls(rule)
      // generate case class representing signature
      val tyName = Type.Name(rule)
      caseClasses += q"@diffable case class $tyName(..${decl.parameters.map(genCaseClassParam).toList})"
//      println(caseClasses.last)

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

    // read file
    val lines = Source.fromFile(s"souffle-importer/minijavac/$filename").getLines
    objects(rule) = mutable.ListBuffer()
    val paramTypes = decl.parameters.map(_.typ)
    // TODO need to know that specific type is type alias for string
    lines.foreach { line =>
      val elems = line.split(delimiter)
      objects(rule) += q"${Term.Name(rule)}(..${elems.zip(paramTypes).map { case (x, y) => compileScalaTerm(x, y) }.toList})"
    }

    case Output(rule) =>

    case PrintSize(rule) =>
  }

  def compileScalaTerm(elem: String, typ: Syntax.Type): Term = typ match {
    case DeclaredType(name) => Lit.String(elem)
    case SymbolType => Lit.String(elem)
    case NumberType => Lit.Int(elem.toInt)
    case UnsignedType => Lit.Long(elem.toLong)
    case FloatType => Lit.Double(elem.toDouble)
  }


  def compile(param: RuleParameter): Param =
    Param(cleanSouffleName(param.name), compile(param.typ))

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

  def  cleanSouffleName(s: String): String = s match {
    //    case "class" => "clazz"
    //    case "var" => "vari"
    //    case "type" => "ty"
    case s if (s.startsWith("?")) => s.tail
    case s => s
  }

  private def genCaseClassParam(param: Syntax.RuleParameter): Term.Param = {
    val name = Term.Name(cleanSouffleName(param.name))
    val ty = genScalaType(param.typ)
    param"$name: $ty"
  }

  private def genScalaType(typ: Syntax.Type): Type = typ match {
    case DeclaredType(name) => Type.Name(name)
    case SymbolType => Type.Name("String")
    case NumberType => Type.Name("Int")
    case UnsignedType => Type.Name("Long")
    case FloatType => Type.Name("Double")
  }

  def compileScalaFile: scala.meta.Source =
    source"""
      package inca.souffle

      object Facts {
        ..${types.toList}
        ..${caseClasses.toList}
        ..${objects.values.flatten.toList}
      }
    """
}
