package inca.frontend.souffle.lowering

import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.core._
import inca.frontend.souffle.Syntax.{Expression => _, Type => _, _}
import inca.frontend.souffle.Util.cleanSouffleName
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.compiler.CompiledSouffleFrontendModule
import inca.runtime.context.DataModel.{Link => MLink}
import inca.util.Gensym
import inca.util.Scala
import truechange.{JavaLitType, LitType}

import scala.collection.mutable

class SouffleToIncaFrontendCompiler {

  private val patFuns: mutable.Map[String, FunctionDef] = mutable.Map()

  private val topLevelRules: mutable.ListBuffer[String] = mutable.ListBuffer()
  private val decls: mutable.Map[String, RuleSignature] = mutable.Map()
  private val inputs: mutable.Map[String, Input] = mutable.Map()

  private val printSizes: mutable.ListBuffer[PrintSize] = mutable.ListBuffer()

  val componentDefinitions: mutable.Map[String, ComponentDefinition] = mutable.Map()

  def compile(name: String, analysis: Analysis): CompiledSouffleFrontendModule = {
    analysis.contents.foreach(compile(_, ""))

    val module = Module(Name(name), Seq(), patFuns.values.toSeq)

    new CompiledSouffleFrontendModule(
      module,
      inputs.values.toSeq.map { input => (decls(input.rule), input) },
      printSizes.toSeq,
      FunctionalOptions()
    )
  }

  // if funPrefix != "" we are within a compontent definition that got initialized
  def compile(content: AnalysisContent, funPrefix: String): Unit = content match {
    case cdef@ComponentDefinition(name, contents) =>
      componentDefinitions += name -> cdef

    case ComponentInitialization(name, composite) =>
      val cdef = componentDefinitions.getOrElse(composite, throw new IllegalArgumentException(s"Unknown component definition $composite"))
      cdef.contents.foreach(compile(_, name + "_"))

    case s@RuleSignature(name, parameters, _) =>
      val ty = parameters match {
        case Nil => TUnit
        case p :: Nil => TSet(compile(p.typ))
        case ps => TSet(TTuple(ps.map(p => compile(p.typ))))
      }
      val fun = FunctionDef(Seq(), None, Name(funPrefix + name), Seq(), ty, SetExp(Seq()))
      // this is a top-level rule
      if (funPrefix == "") {
        topLevelRules += name
      }
      patFuns += (funPrefix + name) -> fun
      decls += name -> s

    case ruleDef@RuleDefinition(heads, rulebody) =>
      for (RuleHead(name, args) <- heads) {
        val prefName = funPrefix + name
        val fun = patFuns.getOrElse(prefName, throw new IllegalArgumentException(s"Unknown relation $prefName"))

        val usedVars = collect(ruleDef)
        implicit val gensym: Gensym = new Gensym(usedVars)
        val predicates: Seq[Expression] = rulebody.map(s => compile(s, funPrefix))
        val alt = SetComprehension(Tuple.from(args.map(compile)), predicates)
        val newBody = BaseApplyInfix(fun.body, "++", alt)
        val newFun = FunctionDef(fun.annos, fun.vis, fun.name, fun.params, fun.outType, newBody)
        patFuns += prefName -> newFun
      }

    case TypeDeclaration(name, superType) => // do nothing

    case in@Input(rule, filename, delimiter) =>
      // TODO generate data constructor for rule?


//      val decl = decls(rule)
//      inputs(rule) = in
//      // generate pattern that enumerates all node instances of AST node class
//      val fun = patFuns.getOrElse(rule, throw new IllegalArgumentException("Rule signature has to come before input declaration"))
//      val body = Body(
//        HasType(Var("node"), TNode(rule)) +:
//        decl.parameters.map { param =>
//          val cleanName = cleanSouffleName(param.name)
//          Path(Var("node"), TNode(rule), NamedLink(TNode(rule), cleanName), Var(cleanName), compile(param.typ))
//        }
//      )
//      patFuns(rule) = Pattern(fun.vis, fun.name, fun.params, Seq(body))

    case Output(name) =>
      val prefName = funPrefix + name
      val fun = patFuns.getOrElse(prefName, throw new IllegalArgumentException(s"Unknown relation $prefName"))
      patFuns += prefName -> fun.copy(annos = MainFunctionAnno +: fun.annos)

    case PrintSize(rule) => // do nothing
      printSizes += PrintSize(funPrefix + rule)
  }

  def compile(typ: Syntax.Type): Type = typ match {
    case DeclaredType(_) => TScalaString
    case SymbolType => TScalaString
    case NumberType => TScalaInt
    case UnsignedType => TScalaLong
    case FloatType => TScalaDouble
  }

  def getJavaClassForType(typ: Syntax.Type): Class[_] = typ match {
    case DeclaredType(name) => classOf[java.lang.Integer]
    case SymbolType => classOf[java.lang.Integer]
    case NumberType => classOf[java.lang.Integer]
    case UnsignedType => classOf[java.lang.Long]
    case FloatType => classOf[java.lang.Double]
  }

  def compile(stm: Syntax.Statement, funPrefix: String)(implicit gensym: Gensym): Expression = stm match {
    case Syntax.Equality(left, not, right)  =>
      val op = if (not) "!=" else "=="
      BaseApplyInfix(compile(left), op, compile(right))
    case Syntax.RuleApplication(negated, component, rule, args) =>
      if (negated)
        throw new UnsupportedOperationException("Cannot currently support negation, in " + stm)

      val terms = args.map(compile)
      component match {
        case Some(c) =>
          Call(Var(Name(s"${c}_$rule")), terms)
        case None =>
          val ruleName = if (topLevelRules.contains(rule)) rule else funPrefix + rule
          Call(Var(Name(ruleName)), terms)
      }
  }

  def compile(exp: Syntax.Expression)(implicit gensym: Gensym): Expression = exp match {
    case Syntax.Variable(name) => Var(cleanSouffleName(name))
    case Syntax.StringValue(value) => BaseLit(Scala(scala.meta.Lit.String(value.intern())))
    case Syntax.NumberValue(value) => BaseLit(Scala(scala.meta.Lit.Int(value)))
    case Syntax.Any =>
      val fresh = gensym.fresh("wildcard")
      Var(fresh)
    case Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, arguments) =>
      val emptyString = BaseLit(Scala(scala.meta.Lit.String("")))
      arguments.foldLeft[Expression](emptyString)((e, arg) => BaseApplyInfix(e, "+", compile(arg)))
    case _ => throw new IllegalArgumentException(s"TODO $exp not supported")
  }

  def genLitLinks: Map[MLink, LitType] =
    decls.values.flatMap { decl =>
      decl.parameters.map { param =>
        val link = decl.name -> cleanSouffleName(param.name)
        link -> JavaLitType(getJavaClassForType(param.typ))
      }
    }.toMap

  def collect(rule: RuleDefinition): Set[String] = {
    rule.heads.flatMap(collect).toSet ++ rule.body.flatMap(collect)
  }

  def collect(head: RuleHead): Set[String] = head.arguments.flatMap(collect).toSet

  def collect(exp: Syntax.Expression): Set[String] = exp match {
    case Syntax.Variable(name) => Set(name)
    case Syntax.StringValue(_) => Set()
    case Syntax.NumberValue(_) => Set()
    case Syntax.Any => Set()
    case Syntax.BuiltInFunctionCall(Syntax.CatBuiltInFunction, arguments) =>
      Set("cat") ++ arguments.flatMap(collect)
  }

  def collect(stm: Syntax.Statement): Set[String] = stm match {
    case Syntax.RuleApplication(negated, component, rule, arguments) =>
      Set(rule) ++ arguments.flatMap(collect)
    case Syntax.Equality(left, _, right) => collect(left) ++ collect(right)
    case Syntax.Parens(stm) => collect(stm)
  }
}

