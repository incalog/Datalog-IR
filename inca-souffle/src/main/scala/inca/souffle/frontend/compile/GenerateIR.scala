package inca.souffle.frontend.compile

import inca.ir
import inca.ir.Language
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.{block, aggregate as iragg, arithmetic as irarith, bool as irbool, data as irdata, disjunction as irdis, not as irnot, string as irstring}
import inca.souffle.frontend.compile.{SouffleInputHint, SouffleOutputHint, SouffleQueryPlanHint}
import inca.souffle.syntax.*
import inca.souffle.syntax.ProgramContent.RelationDecl
import inca.util.Gensym

import scala.annotation.tailrec

class GenerateIR:
  trait Context:
    /* These fields are relative to the ComponentInit we are in */

    // The current prefix created by nested ComponentInit calls
    val prefix: Seq[String] = Seq()
    // The current prefix for each Rule or Type declaration
    val declPrefixes: Map[ProgramContent, Seq[String]] = Map()

    /* These fields are independent of the ComponentInit we are in */

    // All rules / facts that belong to a RelationDecl
    val rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = Map()
    // Collect for all RelationDecl if it is an edb relation or not
    val edbDecls: Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] = Map()
    // Collect for all RelationDecl if it is an output or not
    val outputDecls: Set[ProgramContent.RelationDecl] = Set()

    // Create a new context for a prefix and extend it with new decls found for this prefix
    def extend(newPrefix: Seq[String], decls: Seq[ProgramContent]): Context =
      val obj = this
      new Context {
        override val prefix: Seq[String] = newPrefix
        override val declPrefixes: Map[ProgramContent, Seq[String]] = obj.declPrefixes ++ decls.map(_ -> newPrefix).toMap
        override val rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = obj.rules
        override val edbDecls: Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] = obj.edbDecls
        override val outputDecls: Set[ProgramContent.RelationDecl] = obj.outputDecls
      }

  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + irbool.IR + irdata.IR
    + irdis.IR + irnot.IR + irstring.IR
    + iragg.IR
  )
  val gensym: Gensym = new Gensym()


  def compileProgram(prog: Program, name: String): ir.Module =
    val nameResolution = new NameResolution {}
    nameResolution.resolveProgram(prog)

    val ctx = new Context {
      override val prefix: Seq[String] = Seq()
      override val declPrefixes: Map[ProgramContent, Seq[String]] = collectDirectDecls(prog.content).map(_ -> Seq()).toMap
      override val rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = collectRules(prog.content)
      override val edbDecls: Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] = collectEdbDecls(prog.content)
      override val outputDecls: Set[ProgramContent.RelationDecl] = collectOutputDecls(prog.content)
    }

    ir.Module(ir.Name(name), irLang, compileProgramContents(prog.content)(ctx))

  private def collectRules(content: Seq[ProgramContent]): Map[ProgramContent.RelationDecl, Seq[ProgramContent]] =
    var rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = Map()
    content.foreach {
      case rule@ProgramContent.Rule(heads, _, _) =>
        rules ++= heads.map {
          case call: Atom.Call =>
            val relDecl = call.target.get
            val existingRules = rules.getOrElse(relDecl, Seq())
            relDecl -> (existingRules :+ rule)
        }
      case fact@ProgramContent.Fact(_, _) =>
        val relDecl = fact.target.get
        val existingRules = rules.getOrElse(relDecl, Seq())
        rules += relDecl -> (existingRules :+ fact)
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        rules ++= collectRules(compContent)
      case _ => // nothing
    }
    rules

  private def collectEdbDecls(content: Seq[ProgramContent]): Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] =
    // TODO: This assumes, we do not allow cases such as
    //  .decl A, B
    //  .input A
    var edbDecls = Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]]()
    content.foreach {
      case input@ProgramContent.Directive(DirectiveQualifier.Input, _, _) =>
        edbDecls += input.target.get -> input.attrs
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        edbDecls ++= collectEdbDecls(compContent)
      case _ => // nothing
    }
    edbDecls

  private def collectOutputDecls(content: Seq[ProgramContent]): Set[ProgramContent.RelationDecl] =
    // TODO: This assumes, we do not allow cases such as
    //  .decl A, B
    //  .input A
    var outputDecls = Set[ProgramContent.RelationDecl]()
    content.foreach {
      case output@ProgramContent.Directive(DirectiveQualifier.Output, _, _) =>
        outputDecls += output.target.get
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        outputDecls ++= collectOutputDecls(compContent)
      case _ => // nothing
    }
    outputDecls

  private def collectDirectDecls(content: Seq[ProgramContent]): Seq[ProgramContent] =
    content.flatMap {
      case decl: ProgramContent.RelationDecl => Seq(decl)
      case decl: ProgramContent.TypeDecl => Seq(decl)
      case _ => Seq()
    }

  private def compileProgramContents(contents: Seq[ProgramContent])(ctx: Context): Seq[ir.ModuleEntry] =
    contents.flatMap(c => compileProgramContent(c)(ctx))

  private def compileProgramContent(content: ProgramContent)(ctx: Context): Seq[ir.ModuleEntry] =
    given Context = ctx
    content match
      case relDecl@ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
        compileRelationDecl(relDecl)
      case ProgramContent.Rule(heads, body, queryPlan) =>
        Seq() // nothing, handled by ProgramContent.RelationDecl
      case ProgramContent.Fact(name, args) =>
        Seq() // nothing, handled by ProgramContent.RelationDecl
      case typeDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) =>
        compileAdtDecl(typeDecl)
      case typeDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(alts)) =>
        compileUnionTypeDecl(typeDecl)
      case typeDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(alts)) =>
        compileRecordTypeDecl(typeDecl)
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.SubType(ty)) =>
        throw IllegalStateException(s"Subtypes are not supported: $content")
      case ProgramContent.TypeDecl(name, _) =>
        Seq() // nothing
      case ProgramContent.ComponentDecl(_, _, compContent) =>
        Seq() // nothing, handled by ProgramContent.ComponentInit
      case compInit@ProgramContent.ComponentInit(n, compType) =>
        val compDecl@ProgramContent.ComponentDecl(_, _, compContent) = compInit.target.get
        val newCtx = ctx.extend(ctx.prefix :+ n, collectDirectDecls(compContent))
        compileProgramContents(compContent)(newCtx)
      case _ =>
        Seq()
      /*
      case ProgramContent.Directive(dirQualifier, name, attrs) => ???
      case ProgramContent.Override(n) => ???
      case ProgramContent.FunctorDecl(name, params, retType, stateful) => ???
      case ProgramContent.Pragma(option, arg) => ???*/

  private def cleanName(name: String): String =
    name.replace("?", "Q_")

  private def cleanParamName(name: String): ir.Name =
    // We know that $ is disallowed as souffle variable name
    ir.Name(s"${cleanName(name)}$$param")

  private def namesToIrName(ns: Seq[String]): ir.Name =
    ir.Name(ns.map(cleanName).mkString("$"))

  private def prefixedIrName(name: String)(implicit ctx: Context): ir.Name =
    namesToIrName(ctx.prefix :+ name)

  private def prefixedIrName(decl: ProgramContent, qName: QualifiedName)(implicit ctx: Context): ir.Name =
    ctx.declPrefixes.get(decl) match
      case Some(prefix) => namesToIrName(prefix :+ qName.ns.last) // relative name
      case _ => namesToIrName(qName.ns) // absolute name

  private def ruleHasName(rule: ProgramContent, relName: String): Boolean = rule match
    // A single rule can have multiple names. Only a single name must match
    case ProgramContent.Rule(heads, _, _) =>
      heads.exists {
        case Atom.Call(QualifiedName(ns), _) if ns.last == relName => true
        case _ => false
      }
    case ProgramContent.Fact(QualifiedName(ns), args) if ns.last == relName => true
    case _ => false

  private def compileAdtDecl(decl: ProgramContent.TypeDecl)(implicit ctx: Context) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) = decl
    val dataDefName = prefixedIrName(name)
    val dataDef = irdata.DataDefinition(dataDefName)
    val caseDefs = alts.map { case ADTConstructor(caseName, attrs) =>
      irdata.CaseDefinition(prefixedIrName(caseName), attrs.map(a => compileType(a.ty)), irdata.TData(dataDefName))
    }
    dataDef +: caseDefs

  private def compileUnionTypeDecl(decl: ProgramContent.TypeDecl)(implicit ctx: Context) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Express Union types with ADTs
    ???

  private def compileRecordTypeDecl(decl: ProgramContent.TypeDecl)(implicit ctx: Context) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Introduce IR for Record types
    ???

  private def compileRelationDecl(decl: ProgramContent.RelationDecl)(implicit ctx: Context): Seq[ir.ModuleEntry] =
    // TODO: Do something with qualifiers and choiceDomain
    val ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) = decl

    names.map { relName =>
      val params = attrs.map(compileAttribute)

      val edb = ctx.edbDecls.get(decl)
      edb match
        case Some(attrs) =>
          ir.ExtensionalRelation(prefixedIrName(relName), params)
            .addHint(SouffleInputHint(attrs))
        case None =>
          // Find all rules relevant for this relation
          val rulesForRelation = ctx.rules(decl).filter(r => ruleHasName(r, relName))
          val rel = ir.Relation(prefixedIrName(relName), params, rulesForRelation.flatMap {
            case r: ProgramContent.Rule => compileRule(decl, r, relName)
            case f: ProgramContent.Fact => Seq(compileFact(decl, f))
            case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
          })
          if (ctx.outputDecls.contains(decl))
            rel.addHint(SouffleOutputHint)
          rel
    }

  private def compileAttribute(attr: Attribute)(implicit ctx: Context): ir.Param =
    ir.Param(cleanParamName(attr.name), compileType(attr.ty))

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule, relName: String)(implicit ctx: Context): Seq[ir.Body] =
    val ProgramContent.Rule(heads, atom, queryPlanOption) = rule
    // need to consider that there could be multiple heads for the same rule
    // e.g. R(x), R(y) :- Q(x, y).
    val headTermsPerRule = heads.flatMap {
      case Atom.Call(QualifiedName(ns), terms) if ns.last == relName => Some(terms)
      case _ => None
    }
    headTermsPerRule.map { headTerms =>
      // rules might use other variable names or even terms in their head
      val renameAtoms = headTerms.zip(decl.attrs).map { (headTerm, attr) =>
        ir.Eq(compileTerm(headTerm), ir.Var(cleanParamName(attr.name)))
      }
      val body = ir.Body(compileAtom(atom) +: renameAtoms)
      queryPlanOption match
        case Some(qp) => body.addHint(SouffleQueryPlanHint(qp))
        case None => body
    }


  private def compileFact(decl: ProgramContent.RelationDecl, fact: ProgramContent.Fact)(implicit ctx: Context): ir.Body =
    val ProgramContent.Fact(name, args) = fact
    ir.Body(
      args.zip(decl.attrs).map { (arg, attr) =>
        ir.Eq(ir.Var(cleanParamName(attr.name)), compileTerm(arg))
      }
    )

  private def compileAtom(atom: Atom)(implicit ctx: Context): ir.Atom = atom match
    case Atom.Not(atom) =>
      irnot.Not(compileAtom(atom))
    case call@Atom.Call(qname, args) =>
      val compileArgs = args.map(compileTermAsArgument)
      val decl = call.target.get
      ctx.edbDecls.get(decl) match
        case Some(_) =>
          ir.ExtensionalCall(prefixedIrName(decl, qname), compileArgs)
        case None =>
          ir.Call(prefixedIrName(decl, qname), compileArgs)
    case Atom.Disjunction(bodys) =>
      irdis.Disjunction(bodys.map(atoms => irdis.DisjunctionAlternative(atoms.map(compileAtom))))
    case Atom.Compare(t1, Comparator.EQ, t2) =>
      ir.Eq(compileTerm(t1), compileTerm(t2))
    case Atom.Compare(t1, Comparator.NEQ, t2) =>
      ir.Eq(compileTerm(t1), compileTerm(t2), true)
    case Atom.Compare(t1, op, t2) =>
      irarith.BinCompare(compileTerm(t1), compileTerm(t2), op.toString)
    case Atom.Match(t1, t2) => ???
    case Atom.Contains(t1, t2) => ???
    case Atom.True => ir.Eq(BoolTrue, BoolTrue)
    case Atom.False => ir.Eq(BoolTrue, BoolFalse)

  private def compileTermAsArgument(term: Term)(implicit ctx: Context): ir.Arg = term match
    case Term.Var("_") => ir.WildcardArg() // Souffle only allows wildcards at argument positions
    case _ => compileTerm(term).arg

  private def compileTerm(term: Term)(implicit ctx: Context): ir.Term = term match
    case Term.Var(name) => ir.Var(ir.Name(cleanName(name)))
    case Term.StringLit(s) => irstring.StringLit(s)
    case Term.NumberLit(n) => irarith.IntNum(n)
    case Term.UnsignedLit(n) => irarith.IntNum(n.toInt)
    case Term.FloatLit(f) => irarith.DoubleNum(f)
    case Term.Nil() => ???
    case Term.List(s) => ???
    case constr@Term.Constr(qname, args) =>
      val decl = constr.target.get
      irdata.Construct(prefixedIrName(decl, qname), args.map(compileTerm))
    case Term.TypeCast(t, ty) =>
      ir.Cast(compileTerm(t), compileType(ty))
    case Term.AggregatorTerm(agg) => ???
    case Term.IntrinsicFunctorApp(f, args) =>
      f match
        case IntrinsicFunctor.Ord => ???
        case IntrinsicFunctor.ToFloat => ???
        case IntrinsicFunctor.ToNumber => ???
        case IntrinsicFunctor.ToString => ???
        case IntrinsicFunctor.ToUnsigned => ???
        case IntrinsicFunctor.Cat =>
          val lhs = args.head
          val rhs = args(1)
          irstring.StringConcat(compileTerm(lhs), compileTerm(rhs))
        case IntrinsicFunctor.StrLen => ???
        case IntrinsicFunctor.Substr => ???
        case IntrinsicFunctor.Max =>
          val lhs = args.head
          val rhs = args(1)
          irarith.Max(compileTerm(lhs), compileTerm(rhs))
        case IntrinsicFunctor.Min =>
          val lhs = args.head
          val rhs = args(1)
          irarith.Min(compileTerm(lhs), compileTerm(rhs))
    case Term.UserDefFunctorApp(f, args) => ???

    case Term.Unary(UnOp.Neg, t) => irarith.Neg(compileTerm(t))
    case Term.Unary(UnOp.Bnot, t) => ???
    case Term.Unary(UnOp.Lnot, t) => irbool.BoolNot(compileTerm(t))

    case Term.Binary(t1, BinOp.Add, t2) => irarith.Add(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Sub, t2) => irarith.Sub(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Mul, t2) => irarith.Mul(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Div, t2) => irarith.Div(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Rem, t2) => ???
    case Term.Binary(t1, BinOp.Pow, t2) => ???
    case Term.Binary(t1, BinOp.Land, t2) => irbool.BoolAnd(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lor, t2) => irbool.BoolOr(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lxor, t2) =>
      val a = compileTerm(t1)
      val b = compileTerm(t2)
      irbool.BoolAnd(irbool.BoolOr(a, b), irbool.BoolNot(irbool.BoolAnd(a, b)))
    case Term.Binary(t1, BinOp.Band, t2) => ???
    case Term.Binary(t1, BinOp.Bor, t2) => ???
    case Term.Binary(t1, BinOp.Bxor, t2) => ???
    case Term.Binary(t1, BinOp.Bshl, t2) => ???
    case Term.Binary(t1, BinOp.Bshr, t2) => ???
    case Term.Binary(t1, BinOp.Bshru, t2) => ???

  @tailrec
  private def compileType(ty: Type)(implicit ctx: Context): ir.Type = ty match
    case Type.Number => irarith.TInt
    case Type.Symbol => irstring.TString
    case Type.Unsigned => irarith.TInt
    case Type.Float => irarith.TDouble
    case nameTy@Type.Name(qname) =>
      nameTy.target.get match
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.DefType()) =>
          irstring.TString
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.EqType(eTy)) =>
          compileType(eTy)
        case decl@ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(_)) =>
          irdata.TData(prefixedIrName(decl, qname))
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(_)) =>
          ???
          //irdata.TData(prefixedIrName(decl, qname))
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(_)) =>
          ???
          // irdata.TData(qNameToPrefixedIrName(qname))
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.SubType(_)) =>
          ???
