package inca.frontend.souffle.lowering

import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog.{Name => _, _}
import inca.backend.optimize.EliminateAliases
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.frontend.souffle.Syntax
import inca.frontend.souffle.Syntax.{Type => _, _}
import inca.frontend.souffle.Util.cleanSouffleName
import inca.frontend.souffle.compiler.CompiledSouffleModule
import inca.runtime.context.DataModel
import inca.runtime.context.DataModel.{Link => MLink}
import inca.util.{Gensym, Scala}
import truechange.{JavaLitType, LitType}

import scala.collection.immutable.MultiDict
import scala.collection.mutable
import scala.meta.{Input => _, Name => _, Term => _, Type => _, _}

class SouffleToDatalogIR {

  private val patterns: mutable.SeqMap[String, Pattern] = mutable.SeqMap()

  private val topLevelRules: mutable.ListBuffer[Name] = mutable.ListBuffer()
  private val decls: mutable.Map[Name, RuleSignature] = mutable.Map()
  private val inputs: mutable.Map[Name, Input] = mutable.Map()

  private val printSizes: mutable.ListBuffer[PrintSize] = mutable.ListBuffer()

  val componentDefinitions: mutable.Map[Name, ComponentDefinition] = mutable.Map()

  def compile(name: String, souffle: SouffleModule): CompiledSouffleModule = {
    souffle.contents.foreach(compile(_, ""))

    val module = Module(name, Seq(), patterns.values.toSeq, Seq())
    val moduleWithUnbounded = PropagateUnbounded.transformModule(module)
    printSizes.foreach { ps =>
      moduleWithUnbounded.pats.find(_.name == ps.name.name).foreach { pat =>
        pat.addHint(MagicSetHints.Main(pat.params.map(_ => false)))
      }
    }

    val lang = new DataModel(Set(), MultiDict(), Map(), genLitLinks)

    val moduleWithoutAliases = EliminateAliases.optimizer(lang).optimizeModule(moduleWithUnbounded)

    CompiledSouffleModule(
      souffle,
      moduleWithoutAliases,
      inputs.map { case (name, input) => name.name -> (decls(input.rule), input) }.toMap,
      printSizes.toSeq,
      lang,
      ConstraintOptions()
    )
  }

  // if funPrefix != "" we are within a compontent definition that got initialized
  def compile(content: SouffleContent, funPrefix: String): Unit = content match {
    case cdef@ComponentDefinition(name, contents) =>
      componentDefinitions += name -> cdef

    case ComponentInitialization(name, composite) =>
      val cdef = componentDefinitions.getOrElse(composite, throw new IllegalArgumentException(s"Unknown component definition $composite"))
      cdef.contents.foreach(compile(_, name + "_"))

    case s@RuleSignature(name, parameters, _) =>
      val patName = funPrefix + cleanSouffleName(name)
      val params = parameters.zipWithIndex.map { case (p, ix) =>
        Param(cleanSouffleName(p.name), compile(p.typ))
      }
      val fun = Pattern(None, patName, params, Seq())
        .addHint(SourceConstruct.from(s))
      // this is a top-level rule
      if (funPrefix == "") {
        topLevelRules += name
      }
      patterns += (funPrefix + name) -> fun
      decls += name -> s

    case ruleDef@RuleDefinition(heads, rulebody) =>
      for (head@RuleHead(name, args) <- heads) {
        val pat = patterns.getOrElse(funPrefix + name, throw new IllegalArgumentException(s"Unknown relation ${funPrefix + name}"))
        val params = pat.params.map(_.name)
        val headVars = collectNames(head)
        val bodyVars = ruleDef.body.ss.flatMap(collectNames).map(_.name)

        implicit val gensym: Gensym = new Gensym(bodyVars)
        implicit val subst: Map[Name, String] = headVars.map(v => v -> gensym.fresh(cleanSouffleName(v))).toMap

        val headEqs = (pat.params zip args).flatMap { case (param, arg) =>
          val (term, constraints) = compile(arg)
          constraints :+ Compare(EqComparator, Var(param.name), term).addHint(SourceConstruct.from(arg, head -> arg))
        }

        val constraints = rulebody.ss.map(compile(_, funPrefix))
        val funbody = Body(constraints.flatten ++ headEqs).addHint(SourceConstruct.from(head, head -> ruleDef))

        patterns += (funPrefix + name) -> Pattern(pat.vis, pat.name, pat.params, pat.bodies :+ funbody).withHints(pat)
      }

    case TypeDeclaration(name, superType) => // do nothing

    case in@Input(rel, filename, delimiter) =>
      val decl = decls(rel)
      inputs(rel) = in
      // generate pattern that enumerates all node instances of AST node class
      val pat = patterns.getOrElse(rel.name, throw new IllegalArgumentException("Rule signature has to come before input declaration"))
      val body = Body(
        HasType(Var("node"), TNode(rel.name)) +:
        pat.params.zip(decl.parameters).map { case (param, link) =>
          Path(Var("node"), TNode(rel.name), NamedLink(TNode(rel.name), cleanSouffleName(link.name)), Var(param.name), param.typ)
        }
      ).addHint(SourceConstruct.from(in))
      patterns(rel.name) = Pattern(pat.vis, pat.name, pat.params, Seq(body)).withHints(pat)

    case Output(rule) => // do nothing

    case PrintSize(rule) => // do nothing
      printSizes += PrintSize(Name(funPrefix + rule).sourceLocFrom(rule))
  }

  def compile(typ: Syntax.Type): Type = typ match {
    case DeclaredType(_) => TLiteral.String
    case SymbolType => TLiteral.String
    case NumberType => TLiteral.Int
    case UnsignedType => TLiteral.Long
    case FloatType => TLiteral.Double
  }

  def getJavaClassForType(typ: Syntax.Type): Class[_] = typ match {
    case DeclaredType(name) => classOf[java.lang.Integer]
    case SymbolType => classOf[java.lang.Integer]
    case NumberType => classOf[java.lang.Integer]
    case UnsignedType => classOf[java.lang.Long]
    case FloatType => classOf[java.lang.Double]
  }

  def compile(stm: Syntax.Statement, funPrefix: String)(implicit gensym: Gensym, subst: Map[Name, String]): Seq[Atom] = stm match {
    case Equality(left, not, right) if !not =>
      val (lhterm, lhConstraints) = compile(left)
      val (rhterm, rhConstraints) = compile(right)
      lhConstraints ++ rhConstraints :+
        Compare(EqComparator, lhterm, rhterm)
          .addHint(SourceConstruct.from(stm))
    case Equality(left, not, right) if not =>
      val (lhterm, lhConstraints) = compile(left)
      val (rhterm, rhConstraints) = compile(right)
      lhConstraints ++ rhConstraints :+
        Compare(NeqComparator, lhterm, rhterm)
          .addHint(SourceConstruct.from(stm))
    case RelationApplication(negated, component, rule, args) =>
      val (terms, constraints) = args.map(compile).unzip
      val call = component match {
        case Some(c) => Call(s"${c}_$rule", terms, transitive = false, neg = negated)
        case None =>
          val ruleName = if (topLevelRules.contains(rule)) rule.name else funPrefix + rule.name
          Call(ruleName, terms, transitive = false, neg = negated)
      }
      constraints.flatten :+ call
        .addHint(SourceConstruct.from(stm))
  }

  def compile(exp: Syntax.Expression)(implicit gensym: Gensym, subst: Map[Name, String]): (Term, Seq[Atom]) = exp match {
    case Variable(name) => subst.get(name) match {
      case Some(dlogName) => (Var(dlogName), Seq())
      case None => (Var(cleanSouffleName(name)), Seq())
    }
    case StringValue(value) =>
       (Constant(base.StringLiteral(value.intern)), Seq())
//      val trgVar = Var(gensym.fresh("trg"))
//      val funString = "\"" + value + "\".intern"
//      val computed = Computed(trgVar, ConstantEvaluation(TUnbounded(TString), funString))
//      (trgVar, Seq(computed))
    case NumberValue(value) => (Constant(base.IntLiteral(value)), Seq())
    case Syntax.Wildcard =>
      val fresh = gensym.fresh("wildcard")
      (Var(fresh), Seq())
    case BuiltInFunctionCall(CatBuiltInFunction, arguments) =>
      val params = collectParams(exp)
      val trgVar = Var(gensym.fresh("trg"))
      val typedParams = params.map {
        case Var(name) => param"${scala.meta.Term.Name(name)}: String"
      }.toList
      val funString = q"(..$typedParams) => (${compileEval(exp)}).intern"
      val computed = Computed(trgVar, Evaluation(params.map((_, TLiteral.String)), base.TScalaString, Scala(funString)))
        .addHint(SourceConstruct.from(exp))
      (trgVar, Seq(computed))
    case _ => throw new IllegalArgumentException(s"TODO $exp not supported")
  }

  def collectParams(exp: Syntax.Expression): Seq[Term] = exp match {
    case Variable(name) => Seq(Var(cleanSouffleName(name)))
    case StringValue(_) => Seq()
    case NumberValue(_) => Seq()
    case BuiltInFunctionCall(_, args) => args.flatMap(collectParams)
    case Syntax.Wildcard => throw new IllegalArgumentException("Any is not supported in BuiltInFunctionCall")
  }

  def compileEval(exp: Syntax.Expression): meta.Term = exp match {
    case Variable(name) => scala.meta.Term.Name(cleanSouffleName(name))
    case StringValue(value) =>  scala.meta.Lit.String(value)
    case NumberValue(value) => scala.meta.Lit.String(value.toString)
    case BuiltInFunctionCall(CatBuiltInFunction, args) =>
      val lhs = compileEval(args.head)
      val rhs = compileEval((args(1)))
      q"$lhs + $rhs"
    case Syntax.Wildcard => throw new IllegalArgumentException("Any is not supported in BuiltInFunctionCall")
  }

  def genLitLinks: Map[MLink, LitType] =
    decls.values.flatMap { decl =>
      decl.parameters.map { param =>
        val link = decl.name.name -> cleanSouffleName(param.name)
        link -> JavaLitType(getJavaClassForType(param.typ))
      }
    }.toMap
}

