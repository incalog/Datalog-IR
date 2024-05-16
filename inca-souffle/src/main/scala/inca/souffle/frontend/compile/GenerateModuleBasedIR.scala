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


class GenerateModuleBasedIR:
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + irbool.IR + irdata.IR
    + irdis.IR + irnot.IR + irstring.IR
    + iragg.IR
  )
  val gensym: Gensym = new Gensym()

  var rules: Map[ProgramContent.RelationDecl, Set[ProgramContent]] = Map()
  // Collect for all RelationDecl if it is an edb relation or not
  var edbDecls: Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] = Map()
  // Collect for all RelationDecl if it is an output or not
  var outputDecls: Set[ProgramContent.RelationDecl] = Set()
  // all path for each declaration
  var paths: Map[ProgramContent.RelationDecl, Seq[ComponentType]] = Map()
  // for each component decl store the name and the actual decl that is required
  var requiredDecls: Map[ComponentDecl, Set[(String, ProgramContent.RelationDecl)]] = Map()

  var componentModules: Map[ir.Name, ir.Module] = Map()

  def compileProgram(prog: Program, name: String): Seq[ir.Module] =
    val nameResolution = new NameResolution {}
    nameResolution.resolveProgram(prog)

    val reqAna = new RequirementAnalysis {}
    reqAna.analyseProgram(prog)
    //println(reqAna.requiredDecls.map(a => a._1.ty.n -> a._2.map(_._1)))

    paths = collectPath(prog.content)
    rules = collectRules(prog.content)
    edbDecls = collectEdbDecls(prog.content)
    outputDecls = collectOutputDecls(prog.content)
    requiredDecls = reqAna.requiredDecls

    val content = compileProgramContents(prog.content)
    val souffleModule = ir.Module(ir.Name(name), irLang, content)

    souffleModule +: componentModules.values.toSeq

  private def combineIterables[K, V](a: Map[K, Set[V]], b: Map[K, Set[V]]): Map[K, Set[V]] = {
    a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set.empty)) }
  }

  // TODO: This is wrong we must not collect everything from subcomponents
  private def collectRules(content: Seq[ProgramContent], collectedComponents: Set[ComponentType] = Set()): Map[ProgramContent.RelationDecl, Set[ProgramContent]] =
    // TODO: We can improve this by visiting components only once. We still need to visit them on init!
    var rules: Map[ProgramContent.RelationDecl, Set[ProgramContent]] = Map()
    content.foreach {
      case rule@ProgramContent.Rule(heads, _, _) =>
        val newRules = heads.map {
          case call: Atom.Call =>
            val relDecl = call.target.get
            val existingRules = rules.getOrElse(relDecl, Set())
            relDecl -> (existingRules + rule)
          case a =>
            throw IllegalStateException(s"Unexpected head atom $a")
        }.toMap
        rules = combineIterables(rules, newRules)
      case fact@ProgramContent.Fact(_, _) =>
        val (relDecl, _) = fact.target.get
        val existingRules = rules.getOrElse(relDecl, Set())
        rules += relDecl -> (existingRules + fact)
      case compInit@ProgramContent.ComponentInit(_, compType) =>
        // we must only collect rules in a component, if there is an init for the component
        val compDecl = compInit.target.get
        val newRules = collectRules(compDecl.content)
        rules = combineIterables(rules, newRules)
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

  private def collectPath(content: Seq[ProgramContent], path: Seq[ComponentType] = Seq()): Map[ProgramContent.RelationDecl, Seq[ComponentType]] =
    var declToPath: Map[ProgramContent.RelationDecl, Seq[ComponentType]] = Map()
    content.foreach {
      case decl: RelationDecl =>
        declToPath += decl -> path
      case compDecl@ProgramContent.ComponentDecl(compTy, _, compContent) =>
        declToPath ++= collectPath(compContent, path :+ compTy)
      case _ => // nothing
    }
    declToPath

  implicit def ordering[A <: ProgramContent]: Ordering[A] = (x: A, y: A) => (x, y) match
    case (_: ProgramContent.ComponentDecl, _: ProgramContent.ComponentDecl) => 0
    case (_, _: ProgramContent.ComponentDecl) => -1
    case (_: ProgramContent.ComponentDecl, _) => 1
    case _ => 0

  private def compileProgramContents(contents: Seq[ProgramContent]): Seq[ir.ModuleEntry] =
    contents.sorted.flatMap(compileProgramContent).distinct

  // TODO: transitively collect all requirements
  //  Either A contain inner component B (or inherits) that is
  //  A requires everything of B. Each requirement must either be satisfied by A itself or must be required by a
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

        // collect all relation declarations defined in this component
        val directRels = decl.content.collect { case r: ProgramContent.RelationDecl => r }.toSet
        // collect all requirements, since we need to compile rules for these as well
        val req = requiredDecls(decl)
        val rels = (directRels ++ req.map(_._2)).toSeq

        val content = rels.flatMap(compileRelationDecl) ++ compileProgramContents(decl.content)
        currentComponent = oldComponent

        val (required, provided) = req.toSeq.map {
          case (name, decl) =>
            val params = decl.attrs.map(compileAttribute)
            (RequireRelation(ir.Name("super$" + name), params), ProvideRelation(ir.Name(name), params))
        }.unzip

        val compModule = ir.Module(modName, irLang, content ++ required ++ provided)
        componentModules += modName -> compModule
        compModule

  private var currentComponent: Option[ComponentDecl] = None

  private def compileProgramContent(content: ProgramContent): Seq[ir.ModuleEntry] =
    content match
      case relDecl@ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
        if currentComponent.nonEmpty then
          // we compile this when we compile a component
          Seq()
        else
          // compile the main content
          compileRelationDecl(relDecl)
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
      /*
      case ProgramContent.Directive(dirQualifier, name, attrs) => ???
      case ProgramContent.Override(n) => ???
      case ProgramContent.FunctorDecl(name, params, retType, stateful) => ???
      case ProgramContent.Pragma(option, arg) => ???*/

  private def resolveImport(compTyp: ComponentType, as: String): Seq[ir.Import] =
    if currentComponent.contains(compTyp) then
      Seq()
    else
      val requiredInComponent = requiredDecls(compTyp.target.get).toSeq
      var dependencies: Set[ComponentType] = Set()
      val subst = requiredInComponent.map {
        case (name, decl: RelationDecl) =>
          val compTy = currentComponent.map(_.ty)
          val fromPath = if paths(decl).lastOption == compTy then Seq() else paths(decl)
          dependencies ++= fromPath
          val params = decl.attrs.map(compileAttribute)
          val relName = ir.Name("super$"+name)
          val qualifiedFromName = fromPath.map(compTy => ir.Name(compTy.n)) :+ ir.Name(name)
          RelationSubstitution(relName, params, qualifiedFromName, params)
      }
      val imp = ir.Import(ir.Name(compTyp.n), ir.Name(as), subst)
      dependencies.toSeq.flatMap(dep => resolveImport(dep, dep.n)) :+ imp

  private def cleanName(name: String): String =
    name.replace("?", "Q_")

  private def cleanParamName(name: String): ir.Name =
    // We know that $ is disallowed as souffle variable name
    ir.Name(s"${cleanName(name)}$$param")

  private def namesToIrName(ns: Seq[String]): ir.Name =
    ir.Name(ns.map(cleanName).mkString("$"))

  private def ruleHasName(rule: ProgramContent, relName: String): Boolean = rule match
    // A single rule can have multiple names. Only a single name must match
    case ProgramContent.Rule(heads, _, _) =>
      heads.exists {
        case Atom.Call(QualifiedName(ns), _) if ns.last == relName => true
        case _ => false
      }
    case ProgramContent.Fact(QualifiedName(ns), args) if ns.last == relName => true
    case _ => false

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

      val edb = edbDecls.get(decl)
      edb match
        case Some(attrs) =>
          ir.ExtensionalRelation(ir.Name(relName), params)
            .addHint(SouffleInputHint(attrs))
        case None =>
          // Find all rules relevant for this relation
          val rulesForRelation = rules(decl).filter(r => ruleHasName(r, relName)).toSeq
          val bodies = rulesForRelation.flatMap {
            case r: ProgramContent.Rule if r.target == currentComponent =>
              compileRule(decl, r, relName)
            case f: ProgramContent.Rule => Seq()
              // Call parent impl
              Seq(ir.Body(Seq(
                ir.Call(ir.Name(s"super$$$relName"), decl.attrs.map(compileAttribute).map(p => ir.Var(p.name).arg))
              )))
            case f: ProgramContent.Fact if f.target.get._2 == currentComponent =>
              val factBody = compileFact(decl, f)
              Seq(factBody)
            case f: ProgramContent.Fact => Seq() // nothing
            case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
          }

          // the declaration is in a parent, that is do a super call
          val superCall = if decl.target != currentComponent then
            // Call parent impl
            Some(ir.Body(Seq(
              ir.Call(ir.Name(s"super$$$relName"), decl.attrs.map(compileAttribute).map(p => ir.Var(p.name).arg))
            )))
          else
            None

          val rel = ir.Relation(ir.Name(relName), params, bodies ++ superCall)
          if (outputDecls.contains(decl))
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

      edbDecls.get(decl) match
        case Some(_) =>
          ir.ExtensionalCall(ir.RefByQualifiedName(qname.ns.map(ir.Name.apply)), compileArgs, false)
        case None =>
          ir.Call(ir.RefByQualifiedName(qname.ns.map(ir.Name.apply)), compileArgs, false)
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
    case Term.Nil() => ???
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
