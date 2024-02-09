package inca.souffle.frontend.compile

import inca.ir
import inca.ir.Language
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.{block, aggregate as iragg, arithmetic as irarith, bool as irbool, data as irdata, disjunction as irdis, not as irnot, string as irstring}
import inca.souffle.frontend.SouffleQueryPlanHint
import inca.souffle.frontend.compile.GenerateIR.WILDCARD
import inca.souffle.syntax.*
import inca.util.Gensym

object GenerateIR:
  val WILDCARD = "WILDCARD$"

class GenerateIR {
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + irbool.IR + irdata.IR
    + irdis.IR + irnot.IR + irstring.IR
    + iragg.IR
  )
  val gensym: Gensym = new Gensym()

  var types: Map[ProgramContent.TypeDecl, Type] = Map()
  var rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = Map()

  var contentPrefixes: Map[ProgramContent, Seq[String]] = Map()
  var edbDecls: Set[ProgramContent.RelationDecl] = Set()

  def compileProgram(prog: Program, name: String): ir.Module =
    val nameResolution = new NameResolution {}
    nameResolution.resolveProgram(prog)

    contentPrefixes = collectPrefixes(prog.content)
    rules = collectRules(prog.content)
    edbDecls = collectEdbDecls(prog.content)
    types = collectTypes(prog.content)

    ir.Module(ir.Name(name), irLang, compileProgramContents(prog.content))

  private def collectPrefixes(content: Seq[ProgramContent], prefix: Seq[String] = Seq()): Map[ProgramContent, Seq[String]] =
    var declToPrefix: Map[ProgramContent, Seq[String]] = Map()
    content.foreach {
      case decl: ProgramContent.RelationDecl =>
        declToPrefix += (decl -> prefix)
      case decl: ProgramContent.TypeDecl =>
        declToPrefix += (decl -> prefix)
      case compInit@ProgramContent.ComponentInit(n, compType) =>
        val compDecl@ProgramContent.ComponentDecl(_, _, compContent) = compInit.target.get
        declToPrefix ++= collectPrefixes(compContent, prefix :+ n)
      case _ => // nothing
    }
    declToPrefix

  private def collectTypes(content: Seq[ProgramContent]): Map[ProgramContent.TypeDecl, Type] =
    var types: Map[ProgramContent.TypeDecl, Type] = Map()
    content.foreach {
      case tyDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.DefType()) =>
        // User defined types
        types += tyDecl -> Type.Symbol
      case tyDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) =>
        val adtTy = Type.Name(QualifiedName(Seq(name)))
        adtTy.resolved(tyDecl)
        types += tyDecl -> adtTy
      case tyDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) =>
        val unionTy = Type.Name(QualifiedName(Seq(name)))
        unionTy.resolved(tyDecl)
        types += tyDecl -> unionTy
      case tyDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(alts)) =>
        val recordTy = Type.Name(QualifiedName(Seq(name)))
        recordTy.resolved(tyDecl)
        types += tyDecl -> recordTy
      case tyDecl@ProgramContent.TypeDecl(name, TypeDeclConstraint.EqType(ty)) =>
        types += tyDecl -> ty
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        types ++= collectTypes(compContent)
      case _ => // nothing
    }
    types

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

  private def collectEdbDecls(content: Seq[ProgramContent]): Set[ProgramContent.RelationDecl] =
    // TODO: This assumes, we do not allow cases such as
    //  .decl A, B
    //  .input A
    var edbDecls: Set[ProgramContent.RelationDecl] = Set()
    content.foreach {
      case input@ProgramContent.Directive(DirectiveQualifier.Input, _, _) =>
        edbDecls += input.target.get
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        edbDecls ++= collectEdbDecls(compContent)
      case _ => // nothing
    }
    edbDecls

  private def compileProgramContents(contents: Seq[ProgramContent]): Seq[ir.ModuleEntry] =
    contents.flatMap(compileProgramContent)

  private def compileProgramContent(content: ProgramContent): Seq[ir.ModuleEntry] =
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
        compileProgramContents(compContent)
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

  private def qualifiedNameToIrName(qn: QualifiedName): ir.Name =
    namesToIrName(qn.ns)

  private def ruleHasName(rule: ProgramContent, relName: String): Boolean = rule match
    // A single rule can have multiple names. Only a single name must match
    case ProgramContent.Rule(heads, _, _) =>
      heads.exists {
        case Atom.Call(QualifiedName(ns), _) if ns.last == relName => true
        case _ => false
      }
    case ProgramContent.Fact(QualifiedName(ns), args) if ns.last == relName => true
    case _ => false

  private def isEdbDecl(decl: ProgramContent.RelationDecl): Boolean =
    edbDecls.contains(decl)

  private def compileAdtDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) = decl
    val prefix = contentPrefixes(decl)
    val dataDefName = namesToIrName(prefix :+ name)
    val dataDef = irdata.DataDefinition(dataDefName)
    val caseDefs = alts.map { case ADTConstructor(name, attrs) =>
      irdata.CaseDefinition(namesToIrName(prefix :+ name), attrs.map(a => compileType(a.ty)), irdata.TData(dataDefName))
    }
    dataDef +: caseDefs

  private def compileUnionTypeDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Express Union types with ADTs
    ???

  private def compileRecordTypeDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Introduce IR for Record types
    ???

  private def compileRelationDecl(decl: ProgramContent.RelationDecl): Seq[ir.ModuleEntry] =
    // TODO: Do something with qualifiers and choiceDomain
    val ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) = decl

    names.map { relName =>
      val params = attrs.map(compileAttribute)

      val isExtensionalRelation = isEdbDecl(decl)
      val prefix = contentPrefixes(decl)
      if (isExtensionalRelation) {
        ir.ExtensionalRelation(namesToIrName(prefix :+ relName), params)
      } else {
        // Find all rules relevant for this relation
        val rulesForRelation = rules(decl).filter(r => ruleHasName(r, relName))
        ir.Relation(namesToIrName(prefix :+ relName), params, rulesForRelation.flatMap {
          case r: ProgramContent.Rule => compileRule(decl, r, relName)
          case f: ProgramContent.Fact => Seq(compileFact(decl, f))
          case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
        })
      }
    }

  private def compileAttribute(attr: Attribute): ir.Param =
    ir.Param(cleanParamName(attr.name), compileType(attr.ty))

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule, relName: String): Seq[ir.Body] =
    val ProgramContent.Rule(heads, atom, queryPlanOption) = rule
    if (decl.names.exists(_.contains("isReferenceType")))
      val y = 123

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




  private def compileFact(decl: ProgramContent.RelationDecl, fact: ProgramContent.Fact): ir.Body =
    val ProgramContent.Fact(name, args) = fact
    ir.Body(
      args.zip(decl.attrs).map { (arg, attr) =>
        ir.Eq(ir.Var(cleanParamName(attr.name)), compileTerm(arg))
      }
    )

  private def compileAtom(atom: Atom): ir.Atom = atom match
    case Atom.Not(atom) =>
      irnot.Not(compileAtom(atom))
    case call@Atom.Call(QualifiedName(ns), args) =>
      val compileArgs = args.map(compileTerm).map(_.arg)
      val decl = call.target.get
      val prefix = contentPrefixes(decl)
      val isExtensional = isEdbDecl(decl)
      if (isExtensional)
        ir.ExtensionalCall(namesToIrName(prefix :+ ns.last), compileArgs)
      else
        ir.Call(namesToIrName(prefix :+ ns.last), compileArgs)
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

  private def compileTerm(term: Term): ir.Term = term match
    case Term.Var("_") => ir.Var(gensym.freshName(ir.Name(WILDCARD)))
    case Term.Var(name) => ir.Var(ir.Name(cleanName(name)))
    case Term.StringLit(s) => irstring.StringLit(s)
    case Term.NumberLit(n) => irarith.IntNum(n)
    case Term.UnsignedLit(n) => irarith.IntNum(n.toInt)
    case Term.FloatLit(f) => irarith.DoubleNum(f)
    case Term.Nil() => ???
    case Term.List(s) => ???
    case constr@Term.Constr(name, args) =>
      val decl = constr.target.get
      val prefixes = contentPrefixes(decl)
      irdata.Construct(qualifiedNameToIrName(name), args.map(compileTerm))
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

  private def compileType(ty: Type): ir.Type = ty match
    case Type.Number => irarith.TInt
    case Type.Symbol => irstring.TString
    case Type.Unsigned => irarith.TInt
    case Type.Float => irarith.TDouble
    case nameTy@Type.Name(qn) =>
      val decl = nameTy.target.get
      decl match
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(_)) =>
          val prefix = contentPrefixes(decl)
          irdata.TData(namesToIrName(prefix :+ name))
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(_)) =>
          val prefix = contentPrefixes(decl)
          irdata.TData(namesToIrName(prefix :+ name))
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(_)) =>
          val prefix = contentPrefixes(decl)
          ???
          //irdata.TData(namesToIrName(prefix :+ name))
        case _ =>
          compileType(types(decl))
}
