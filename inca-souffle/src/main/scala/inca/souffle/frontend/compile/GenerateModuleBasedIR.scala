package inca.souffle.frontend.compile

import inca.ir
import inca.ir.{Import, Language, ProvideRelation, RelationSubstitution, Require, RequireRelation}
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.{block, aggregate as iragg, arithmetic as irarith, bool as irbool, data as irdata, disjunction as irdis, not as irnot, string as irstring}
import inca.souffle.frontend.compile.{SouffleInputHint, SouffleOutputHint, SouffleQueryPlanHint}
import inca.souffle.syntax.*
import inca.souffle.syntax.ProgramContent.{ComponentDecl, RelationDecl}
import inca.util.Gensym

import scala.annotation.tailrec

implicit def ordering[A <: ProgramContent]: Ordering[A] = (x: A, y: A) => (x, y) match
  case (_: ProgramContent.ComponentDecl, _: ProgramContent.ComponentDecl) => 0
  case (_, _: ProgramContent.ComponentDecl) => -1
  case (_: ProgramContent.ComponentDecl, _) => 1
  case _ => 0

class GenerateModuleBasedIR extends GenerateIRContext:
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + irbool.IR + irdata.IR
    + irdis.IR + irnot.IR + irstring.IR
    + iragg.IR
  )
  val gensym: Gensym = new Gensym()

  private var componentModules: Map[ir.Name, ir.Module] = Map()

  private var currentComponent: Option[ComponentDecl] = None

  private def currentlyInMainComponent: Boolean = currentComponent.isEmpty

  private def currentlyInComponent(comp: Option[ComponentDecl]): Boolean = currentComponent == comp

  def compileProgram(prog: Program, name: String): Seq[ir.Module] =
    // perform name resolution and prefill the context
    analyseProgram(prog)

    val content = compileProgramContents(prog.content)
    val souffleModule = ir.Module(ir.Name(name), irLang, content)

    souffleModule +: componentModules.values.toSeq


  private def compileProgramContents(contents: Seq[ProgramContent]): Seq[ir.ModuleEntry] =
    contents.sorted.flatMap(compileProgramContent).distinct

  private def collectRelationDecls(componentDecl: ComponentDecl): Set[ProgramContent.RelationDecl] =
    val directDecls = componentDecl.content.collect { case r: ProgramContent.RelationDecl => r }.toSet
    val superComponents = componentDecl.superTys.map(_.target.get)
    directDecls ++ superComponents.flatMap(collectRelationDecls)

  private def compileComponentDecl(decl: ComponentDecl): ir.Module =
    val modName = ir.Name(decl.ty.n)
    componentModules.get(modName) match
      case Some(m) => m
      case _ =>
        // compile all inherited components
        val superDecls = decl.superTys.map(_.target.get)
        superDecls.map(compileComponentDecl)

        val oldComponent = currentComponent
        currentComponent = Some(decl)

        // collect all relation declarations defined in this component or the parent
        val rels = collectRelationDecls(decl).toSeq

        val content = rels.flatMap(compileRelationDecl) ++ compileProgramContents(decl.content)
        currentComponent = oldComponent

        val required = lookupRequiredDeclarations(decl).map {
          case (name, decl) =>
            val params = decl.attrs.map(compileAttribute)
            RequireRelation(ir.Name("super$" + name), params)
        }

        // provide all relations in a component
        val provided = content.collect {
          case ir.Relation(name, params, bodies) => ProvideRelation(name, params)
        }

        val compModule = ir.Module(modName, irLang, content ++ required ++ provided)
        componentModules += modName -> compModule
        compModule

  private def compileProgramContent(content: ProgramContent): Seq[ir.ModuleEntry] =
    content match
      case relDecl@ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
        if currentlyInMainComponent then
          compileRelationDecl(relDecl)
        else
          // we compile this when we compile a component
          Seq()
      case rule@ProgramContent.Rule(heads, body, queryPlan) =>
        Seq()
      case fact@ProgramContent.Fact(name, args) =>
        Seq()
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
      case decl@ProgramContent.ComponentDecl(compTy, _, compContent) if !componentModules.contains(ir.Name(compTy.n)) =>
        Seq() // nothing
      case compInit@ProgramContent.ComponentInit(initName, compTy) =>
        // lazily compile the component and all inherited components if needed
        val decl = compInit.target.get
        compileComponentDecl(decl)
        resolveImport(compTy, initName)
      case _ =>
        Seq()


  private def prefixedRelationName(name: String, decl: RelationDecl, absolutePath: Boolean): ir.Name =
    if absolutePath then
      val requiredRelNames = lookupRequiredDeclarations(decl.target.get).map(_._1)
      if requiredRelNames.contains(name) then
        // the relation is also required in the parent => prefix
        ir.Name("super$" + name)
      else
        // the relation is not required in the parent => no prefix
        ir.Name(name)
    else
      if currentlyInComponent(decl.target) then
        // the relation is declared in the current component => no prefix
        ir.Name(name)
      else
        // the relation is passed down from a parent to the current component and reexported => prefix
        ir.Name("super$" + name)

  private def resolveImport(compTyp: ComponentType, as: String): Seq[ir.Import] =
    val requiredInComponent = lookupRequiredDeclarations(compTyp.target.get)
    var dependencies: Set[ComponentType] = Set()
    val subst = requiredInComponent.map {
      case (name, decl: RelationDecl) =>
        // when we are in the current component, we don't need a prefix for the export
        val fromPath = if currentlyInComponent(decl.target) then Seq() else lookupPath(decl)
        val fromName = prefixedRelationName(name, decl, absolutePath = fromPath.nonEmpty)
        dependencies ++= fromPath
        val params = decl.attrs.map(compileAttribute)
        val relName = ir.Name("super$"+name)
        val qualifiedFromName = fromPath.map(compTy => ir.Name(compTy.n)) :+ fromName
        RelationSubstitution(relName, params, qualifiedFromName, params)
    }
    val imp = ir.Import(ir.Name(compTyp.n), ir.Name(as), subst.toSeq)
    dependencies.toSeq.flatMap(dep => resolveImport(dep, dep.n)) :+ imp

  private def cleanName(name: String): String =
    name.replace("?", "Q_")

  private def cleanParamName(name: String): ir.Name =
    // We know that $ is disallowed as souffle variable name
    ir.Name(s"${cleanName(name)}$$param")

  private def namesToIrName(ns: Seq[String]): ir.Name =
    ir.Name(ns.map(cleanName).mkString("$"))

  private def compileAdtDecl(decl: ProgramContent.TypeDecl) = ???
    /*val ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) = decl
    val dataDefName = prefixedIrName(name)
    val dataDef = irdata.DataDefinition(dataDefName)
    val caseDefs = alts.map { case ADTConstructor(caseName, attrs) =>
      irdata.CaseDefinition(prefixedIrName(caseName), attrs.map(a => compileType(a.ty)), irdata.TData(dataDefName))
    }
    dataDef +: caseDefs*/

  private def compileUnionTypeDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Express Union types with ADTs
    ???

  private def compileRecordTypeDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Adapt record IR to contain nil case and use the extension here
    ???

  private def compileRelationDecl(decl: ProgramContent.RelationDecl): Seq[ir.ModuleEntry] =
    // TODO: Do something with qualifiers and choiceDomain
    val ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) = decl

    names.map { relName =>
      val params = attrs.map(compileAttribute)

      if isEdbDeclaration(decl) then
        ir.ExtensionalRelation(ir.Name(relName), params)
          .addHint(SouffleInputHint(lookupEdbAttributes(decl)))
      else
        // Find all rules relevant for this relation
        val rulesForRelation = lookupRules(relName, decl)
        val bodies = rulesForRelation.flatMap {
          case r: ProgramContent.Rule if currentlyInMainComponent =>
            // we always compile in the main component
            compileRule(decl, r, relName)
          case r: ProgramContent.Rule if currentlyInComponent(r.target) =>
            compileRule(decl, r, relName)
          case f: ProgramContent.Rule => Seq()
            // Call parent impl
            Seq(ir.Body(Seq(
              ir.Call(ir.Name(s"super$$$relName"), decl.attrs.map(compileAttribute).map(p => ir.Var(p.name).arg))
            )))
          case f: ProgramContent.Fact if currentlyInMainComponent =>
            // we always compile in the main component
            Seq(compileFact(decl, f))
          case f: ProgramContent.Fact if currentlyInComponent(f.target.get._2) =>
            Seq(compileFact(decl, f))
          case f: ProgramContent.Fact => Seq() // nothing
          case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
        }

        // the declaration is in a parent, that is do a super call
        val superCall = if !currentlyInComponent(decl.target) then
          // Call parent impl
          Some(ir.Body(Seq(
            ir.Call(ir.Name(s"super$$$relName"), decl.attrs.map(compileAttribute).map(p => ir.Var(p.name).arg))
          )))
        else
          None

        val rel = ir.Relation(ir.Name(relName), params, bodies ++ superCall)
        if isOutputDeclaration(decl) then
          rel.addHint(SouffleOutputHint)
        rel
    }

  private def compileAttribute(attr: Attribute): ir.Param =
    ir.Param(cleanParamName(attr.name), compileType(attr.ty))

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule, relName: String): Seq[ir.Body] =
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
    case call@Atom.Call(qname, args) =>
      val compileArgs = args.map(compileTermAsArgument)
      val decl = call.target.get

      // find the component in which this call is declared
      val fromPath = qname.path.map(n => ir.Name(n))
      val fromName = prefixedRelationName(qname.unqualifiedName, decl, absolutePath = fromPath.nonEmpty)

      if isEdbDeclaration(decl) then
        ir.ExtensionalCall(ir.RefByQualifiedName(fromPath :+ fromName), compileArgs, false)
      else
        ir.Call(ir.RefByQualifiedName(fromPath :+ fromName), compileArgs, false)
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

  private def compileTermAsArgument(term: Term): ir.Arg = term match
    case Term.Var("_") => ir.WildcardArg() // Souffle only allows wildcards at argument positions
    case _ => compileTerm(term).arg

  private def compileTerm(term: Term): ir.Term = term match
    case Term.Var(name) => ir.Var(ir.Name(cleanName(name)))
    case Term.StringLit(s) => irstring.StringLit(s)
    case Term.NumberLit(n) => irarith.IntNum(n)
    case Term.UnsignedLit(n) => irarith.IntNum(n.toInt)
    case Term.FloatLit(f) => irarith.DoubleNum(f)
    case Term.Nil() => ??? // record nil case
    case Term.List(s) => ???
    case constr@Term.Constr(qname, args) =>
      // TODO: Allow calls with absolut paths
      val dataName = ir.Name(qname.ns.mkString("."))
      irdata.Construct(dataName, args.map(compileTerm))
    case Term.TypeCast(t, ty) =>
      ir.Cast(compileTerm(t), compileType(ty))
    case Term.AggregatorTerm(agg) => ???
    case Term.IntrinsicFunctorApp(f, args) =>
      f match
        case IntrinsicFunctor.Ord => ???
        case IntrinsicFunctor.ToFloat => ???
        case IntrinsicFunctor.ToNumber => ???
        case IntrinsicFunctor.ToString =>
          irstring.ToString(compileTerm(args.head))
        case IntrinsicFunctor.ToUnsigned => ???
        case IntrinsicFunctor.Cat =>
          irstring.StringConcat(compileTerm(args.head), compileTerm(args(1)))
        case IntrinsicFunctor.StrLen => ???
        case IntrinsicFunctor.Substr => ???
        case IntrinsicFunctor.Max =>
          irarith.Max(compileTerm(args.head), compileTerm(args(1)))
        case IntrinsicFunctor.Min =>
          irarith.Min(compileTerm(args.head), compileTerm(args(1)))
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
  private def compileType(ty: Type): ir.Type = ty match
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
          // TODO: Allow calls with absolut paths
          val dataName = ir.Name(qname.ns.mkString("."))
          irdata.TData(dataName)
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(_)) =>
          ???
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(_)) =>
          ???
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.SubType(_)) =>
          ???
