package inca.souffle.frontend.compile

import inca.ir
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.{ExtensionalRelationSubstitution, Import, Language, ProvideRelation, RelationSubstitution, Require, RequireRelation, WildcardArg}
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.block.Block
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.data.{RequireCaseDefinition, RequireDataDefinition}
import inca.ir.extension.{block, aggregate as iragg, arithmetic as irarith, bool as irbool, data as irdata, disjunction as irdis, not as irnot, string as irstring}
import inca.ir.typing.Resolvable
import inca.souffle.frontend.compile.{SouffleInputHint, SouffleOutputHint, SouffleQueryPlanHint}
import inca.souffle.syntax.*
import inca.souffle.syntax.Aggregator.Min
import inca.souffle.syntax.ProgramContent.{ComponentDecl, RelationDecl}
import inca.souffle.syntax.TypeDeclConstraint.ADTType
import inca.util.Gensym

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSet, SortedSet}

implicit def ordering[A <: ProgramContent]: Ordering[A] = (x: A, y: A) => (x, y) match
  case (_: ProgramContent.ComponentDecl, _: ProgramContent.ComponentDecl) => 0
  case (_, _: ProgramContent.ComponentDecl) => -1
  case (_: ProgramContent.ComponentDecl, _) => 1
  case _ => 0

class GenerateIR extends GenerateIRContext:
  val irLang: Language = new Language(Set(ir.BaseIR)
                                      + irarith.IR + block.IR + irbool.IR + irdata.IR
                                      + irdis.IR + irnot.IR + irstring.IR
                                      + iragg.IR
  )
  val gensym: Gensym = new Gensym()

  private var componentModules: Map[ir.Name, ir.Module] = Map()

  private var boundVariables: Set[String] = _

  def compileProgram(prog: Program, name: String): Seq[ir.Module] =
    initContext(prog)
    val content = switchToMainComponent {
      compileProgramContents(prog.content)
    }
    val souffleModule = ir.Module(ir.Name(name), irLang, content)
    souffleModule +: componentModules.values.toSeq

  private def compileProgramContents(contents: Seq[ProgramContent]): Seq[ir.ModuleEntry] =
    contents.sorted.flatMap(compileProgramContent).distinct

  private def transitivelyCollectProgramContent[A](componentDecl: ComponentDecl)(f: PartialFunction[ProgramContent, A]): Set[A] =
    val directDecls = componentDecl.content.collect(f).toSet
    val superComponents = componentDecl.superTys.map(_.target.get)
    directDecls ++ superComponents.flatMap(c => transitivelyCollectProgramContent(c)(f))

  private def compileComponentDecl(decl: ComponentDecl): ir.Module =
    val modName = ir.Name(decl.ty.n)
    componentModules.get(modName) match
      case Some(m) => m
      case _ => compileComponentDeclInternal(modName, decl)

  private def compileComponentDeclInternal(moduleName: ir.Name, decl: ComponentDecl): ir.Module =
    // compile all inherited components
    val superDecls = decl.superTys.map(_.target.get)
    superDecls.map(compileComponentDecl)
    // collect all relation declarations defined in this component or the parent
    val rels = transitivelyCollectProgramContent(decl) { case r: ProgramContent.RelationDecl => r }
    val adts = transitivelyCollectProgramContent(decl) { case adt@ProgramContent.TypeDecl(_, _: ADTType) => adt }
    val content = switchToComponent(decl) {
      val compiledRels = rels.toSeq.flatMap(compileRelationDecl)
      val compiledAdts = adts.toSeq.flatMap(compileAdtDecl)
      compiledAdts ++ compiledRels ++ compileProgramContents(decl.content)
    }

    val required = lookupRequiredDeclarations(decl).flatMap {
      case (name, relDecl: ProgramContent.RelationDecl) =>
        val params = relDecl.attrs.map(compileAttribute)
        val req =
          if isEdbDeclaration(relDecl) then
            ir.RequireExtensionalRelation(ir.Name("super$" + name), params)
          else
            ir.RequireRelation(ir.Name("super$" + name), params)
        Seq(req)
      case (name, adt@ProgramContent.TypeDecl(_, ty: ADTType)) =>
        val dataName = ir.Name("super$" + name)
        val dataDef = irdata.RequireDataDefinition(dataName)
        dataDef +: ty.alts.map { case ADTConstructor(n, attrs) =>
          val args = attrs.map(compileAttribute).map(_.ty)
          irdata.RequireCaseDefinition(ir.Name("super$" + n), args, irdata.TData(dataName))
        }
      case (_, d) => throw IllegalStateException(s"Unsupported declaration: $d")
    }
    // provide all relations in a component
    val provided = content.collect {
      case ir.Relation(name, params, _) => ir.ProvideRelation(name, params)
      case ir.ExtensionalRelation(name, params) => ir.ProvideExtensionalRelation(name, params)
      case irdata.DataDefinition(name) => irdata.ProvideDataDefinition(name)
      case irdata.CaseDefinition(name, args, data) => irdata.ProvideCaseDefinition(name, args, data)
    }

    val compModule = ir.Module(moduleName, irLang, content ++ required ++ provided)
    componentModules += moduleName -> compModule
    compModule

  private def compileProgramContent(content: ProgramContent): Seq[ir.ModuleEntry] =
    content match
      case relDecl: ProgramContent.RelationDecl if currentlyInMainComponent =>
        compileRelationDecl(relDecl)
      case adtDecl@ProgramContent.TypeDecl(_, _: TypeDeclConstraint.ADTType) if currentlyInMainComponent =>
        compileAdtDecl(adtDecl)
      case compInit@ProgramContent.ComponentInit(initName, compTy) =>
        val decl = compInit.target.get
        compileComponentDecl(decl)
        resolveImport(compTy, initName)
      case _ =>
        Seq()

  private def prefixedName[R <: Resolvable[ComponentDecl]](name: QName, resolvable: R, absolutePath: Boolean): ir.Name =
    if absolutePath && declIsRequiredInComponent(QName(Seq(name.ns.last)), resolvable.target.get) then
      // the relation is from a parent and is also required in the parent => prefix
      ir.Name("super$" + name.ns.last)
    else if absolutePath then
      // the relation is from a parent, but is not required in it => no prefix
      ir.Name(name.ns.last)
    else if currentlyInComponent(resolvable.target) then
      // the relation is declared in the current component => no prefix
      ir.Name(name.ns.last)
    else
      // the relation is passed down from a parent to the current component and reexported => prefix
      ir.Name("super$" + name.ns.last)

  private def resolveImport(compTyp: ComponentType, as: String): Seq[ir.Import] =
    val requiredInComponent = lookupRequiredDeclarations(compTyp.target.get)
    var dependencies: Set[ComponentType] = Set()
    val subst = requiredInComponent.flatMap {
      case (name, decl: RelationDecl) =>
        // when we are in the current component, we don't need a prefix for the export
        val fromPath = if currentlyInComponent(decl.target) then Seq() else lookupPath(decl)
        dependencies ++= fromPath
        val fromName = prefixedName(name, decl, absolutePath = fromPath.nonEmpty)
        val params = decl.attrs.map(compileAttribute)
        val relName = ir.Name("super$" + name)
        val qualifiedFromName = fromPath.map(compTy => ir.Name(compTy.n)) :+ fromName
        val relSubst =
          if isEdbDeclaration(decl) then
            ExtensionalRelationSubstitution(relName, params, qualifiedFromName, params)
          else
            RelationSubstitution(relName, params, qualifiedFromName, params)
        Seq(relSubst)
      case (name, decl@ProgramContent.TypeDecl(_, ty: ADTType)) =>
        val fromPath = if currentlyInComponent(decl.target) then Seq() else lookupPath(decl)
        dependencies ++= fromPath
        val dataName = ir.Name("super$" + name)
        val fromDataName = prefixedName(name, decl, absolutePath = fromPath.nonEmpty)
        val qualifiedFromDataName = fromPath.map(compTy => ir.Name(compTy.n)) :+ fromDataName
        val dataSubst = irdata.DataDefinitionSubstitution(dataName, qualifiedFromDataName)
        val caseSubsts = ty.alts.map { case ADTConstructor(cName, attrs) =>
          val caseName = ir.Name("super$" + cName)
          val fromCaseName = prefixedName(QName(Seq(cName)), decl, absolutePath = fromPath.nonEmpty)
          val qualifiedFromCaseName = fromPath.map(compTy => ir.Name(compTy.n)) :+ fromCaseName
          val args = attrs.map(compileAttribute).map(_.ty)
          irdata.CaseDefinitionSubstitution(caseName, args, qualifiedFromCaseName, args)
        }
        dataSubst +: caseSubsts
      case (_, d) => throw IllegalStateException(s"Unsupported declaration: $d")
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

  private def compileAdtDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) = decl
    val dataDefName = ir.Name(name)
    val dataDef = irdata.DataDefinition(dataDefName)
    val caseDefs = alts.map { case ADTConstructor(caseName, attrs) =>
      irdata.CaseDefinition(ir.Name(caseName), attrs.map(a => compileType(a.ty)), irdata.TData(dataDefName))
    }
    dataDef +: caseDefs

  private def compileUnionTypeDecl(decl: ProgramContent.TypeDecl) =
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(tys)) = decl
    // TODO: Express Union types with ADTs
    ???

  private def compileRecordTypeDecl(decl: ProgramContent.TypeDecl) =
    // Lower records to ADTs
    val ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(record)) = decl
    val dataDefName = ir.Name(name)
    val dataDef = irdata.DataDefinition(dataDefName)
    val dataTy = irdata.TData(dataDefName)
    val nilCase = irdata.CaseDefinition(ir.Name(name + "Nil"), Seq(), dataTy)
    val conCase = irdata.CaseDefinition(ir.Name(name + "Cons"), record.attrs.map(a => compileType(a.ty)), dataTy)
    Seq(dataDef, nilCase, conCase)

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
          case f: ProgramContent.Rule => Seq() // nothing
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
    val ProgramContent.Rule(heads, bodyAtom, queryPlanOption) = rule
    // need to consider that there could be multiple heads for the same rule
    // e.g. R(x), R(y) :- Q(x, y).
    val headTermsPerRule = heads.flatMap {
      case Atom.Call(QualifiedName(ns), terms) if ns.last == relName => Some(terms)
      case _ => None
    }
    headTermsPerRule.map { headTerms =>
      boundVariables = Set()

      val compiledBody = compileAtom(bodyAtom)
      // rules might use other variable names or even terms in their head
      val renameAtoms = headTerms.zip(decl.attrs).map { (headTerm, attr) =>
        ir.Eq(compileTerm(headTerm), ir.Var(cleanParamName(attr.name)))
      }

      val body = ir.Body(compiledBody +: renameAtoms)
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
      irnot.WeakNot(compileAtom(atom))
    case call@Atom.Call(qname, args) =>
      val compileArgs = args.map(compileTermAsArgument)
      val decl = call.target.get

      // find the component in which this call is declared
      val fromPath = qname.path.map(n => ir.Name(n))
      val fromName = prefixedName(QName(qname.ns), decl, absolutePath = fromPath.nonEmpty)

      if isEdbDeclaration(decl) then
        ir.ExtensionalCall(ir.RefByQualifiedName(fromPath :+ fromName), compileArgs, false)
      else
        ir.Call(ir.RefByQualifiedName(fromPath :+ fromName), compileArgs, false)
    case Atom.Disjunction(bodys) =>
      irdis.Disjunction(bodys.map(atoms => irdis.DisjunctionAlternative(atoms.map(compileAtom))))
    case Atom.Compare(t1, Comparator.EQ, t2) =>
      def compileDeconstruct(constr: Term.Constr, t: Term): ir.Atom =
        val qname = constr.qualifiedName
        val args = constr.args
        val typeDecl = constr.target.get
        val fromPath = qname.path.map(n => ir.Name(n))
        val fromName = prefixedName(QName(qname.ns), typeDecl, absolutePath = fromPath.nonEmpty)
        irdata.Deconstruct(compileTerm(t), fromPath :+ fromName, args.map(compileTerm(_).arg))

      // Handle deconstructs
      (t1, t2) match
        case (constr: Term.Constr, t) if isBound(t) && !isBound(constr) => compileDeconstruct(constr, t)
        case (t, constr: Term.Constr) if isBound(t) && !isBound(constr) => compileDeconstruct(constr, t)
        case (constr: Term.Constr, t) if isBound(t) && !isBound(constr) => compileDeconstruct(constr, t)
        case (t, constr: Term.Constr) if isBound(t) && !isBound(constr) => compileDeconstruct(constr, t)
        case _ => ir.Eq(compileTerm(t1), compileTerm(t2))

    case Atom.Compare(t1, Comparator.NEQ, t2) =>
      ir.Eq(compileTerm(t1), compileTerm(t2), true)
    case Atom.Compare(t1, op, t2) =>
      irarith.BinCompare(compileTerm(t1), compileTerm(t2), op.toString)
    case Atom.Match(t1, t2) => irstring.RegexMatch(compileTerm(t1), compileTerm(t2))
    case Atom.Contains(t1, t2) => ???
    case Atom.True => ir.Eq(BoolTrue, BoolTrue)
    case Atom.False => ir.Eq(BoolTrue, BoolFalse)

  private def compileTermAsArgument(term: Term): ir.Arg = term match
    case Term.Var("_") => ir.WildcardArg() // Souffle only allows wildcards at argument positions
    case _ => compileTerm(term).arg

  private def isBound(term: Term): Boolean = term match
    case Term.Var(name) => boundVariables.contains(name)
    case Term.Constr(_, args) => args.forall(isBound)
    case Term.TypeCast(t, _) => isBound(t)
    case _ => true

  private def compileArg(term: Term): ir.Arg = term match
    case Term.Var("_") => WildcardArg()
    case _ => compileTerm(term).arg

  private def compileTerm(term: Term): ir.Term = term match
    case Term.Var(name) =>
      // TODO: This over approximates variables. We would need a precise typechecker to do this the correct way.
      //  E.g variables in negative calls are not bound.
      boundVariables += name
      ir.Var(ir.Name(cleanName(name)))
    case Term.StringLit(s) => irstring.StringLit(s)
    case Term.NumberLit(n) => irarith.IntNum(n)
    case Term.UnsignedLit(n) => irarith.IntNum(n.toInt)
    case Term.FloatLit(f) => irarith.DoubleNum(f)
    case Term.Nil() => ??? // record nil case
    case Term.RecordList(s) => ???
    case constr@Term.Constr(qname, args) =>
      // Deconstruct, since all args are bound
      val typeDecl = constr.target.get
      val fromPath = qname.path.map(n => ir.Name(n))
      val fromName = prefixedName(QName(qname.ns), typeDecl, absolutePath = fromPath.nonEmpty)
      irdata.Construct(fromPath :+ fromName, args.map(compileTerm))
    case Term.TypeCast(t, ty) =>
      ir.Cast(compileTerm(t), compileType(ty))
    case Term.AggregatorTerm(agg) =>
      val (incaAggOp, aggCalls, outTerm) = agg match
        case Aggregator.Min(t, args) => (irarith.ArithmeticAggregationOperator.MinInt, args, Some(t))
        case Aggregator.Max(t, args) => (irarith.ArithmeticAggregationOperator.MaxInt, args, Some(t))
        case Aggregator.Sum(t, args) => (irarith.ArithmeticAggregationOperator.SumInt, args, Some(t))
        case Aggregator.Count(args) => (irarith.ArithmeticAggregationOperator.Count, args, None)

      // TODO: We only support aggregations of this form
      val allowedFormat = "agg_op v: Term.Var : { Call(..., v, ...) }"
      val errMsg = s"Only Souffle aggregations of the form: $allowedFormat are allowed"
      if (aggCalls.size != 1)
        throw IllegalStateException(errMsg)


      aggCalls.head match
        case call@Atom.Call(qualifiedName, args) =>
          val relDecl = call.target match
            case Some(decl) => decl
            case _ => throw IllegalArgumentException(s"Unresolved relation declaration for call $call")

          val fromPath = qualifiedName.path.map(n => ir.Name(n))
          val fromName = prefixedName(QName(qualifiedName.ns), relDecl, absolutePath = fromPath.nonEmpty)
          val ref: ir.Ref[ir.Relation] = ir.RefByQualifiedName(fromPath :+ fromName)

          outTerm match
            case Some(t) =>
              val aggIndex = args.indexOf(t)
              if (aggIndex < 0)
                throw IllegalStateException(errMsg)

              val compiledArgs = args.map(compileArg)
              val aggTerm = compileTerm(t)
              val newArgs = compiledArgs.updated(aggIndex, iragg.AggregateColumnArg(aggTerm))
              val aggAtom = iragg.Aggregate(ref, newArgs, incaAggOp)
              Block(aggAtom, aggTerm)
            case _ =>
              // Count aggregation for example does not specify a column to aggregate on.
              // In our IR the aggregated column does exist, but does not matter.
              // We always get the same result, which is the number of rows in the relation minus the filtering.
              val unboundIndex = args.indexWhere(t => !isBound(t) && t.isInstanceOf[Term.Var])
              if (unboundIndex < 0)
                throw IllegalStateException("Aggregation must contain at least one unbound variable.")
              val aggTerm = ir.Var(ir.Name(gensym.fresh("aggCol"))) // ok, since aggregation args do not bind things
              val compiledArgs = args.map(compileArg).updated(unboundIndex, AggregateColumnArg(aggTerm))
              val aggAtom = iragg.Aggregate(ref, compiledArgs, incaAggOp)
              Block(aggAtom, aggTerm)
        case _ =>
          throw IllegalStateException(errMsg)


    case Term.IntrinsicFunctorApp(f, args) =>
      f match
        case IntrinsicFunctor.Ord => irstring.OrdinalNumber(compileTerm(args.head))
        case IntrinsicFunctor.ToFloat => ???
        case IntrinsicFunctor.ToNumber => ???
        case IntrinsicFunctor.ToString =>
          irstring.ToString(compileTerm(args.head))
        case IntrinsicFunctor.ToUnsigned => ???
        case IntrinsicFunctor.Cat =>
          irstring.StringConcat(compileTerm(args.head), compileTerm(args(1)))
        case IntrinsicFunctor.StrLen =>
          irstring.StringLength(compileTerm(args.head))
        case IntrinsicFunctor.Substr =>
          val Seq(s, idx, len) = args.map(compileTerm)
          irstring.Substring(s, idx, len)
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
          val fromPath = qname.path.map(n => ir.Name(n))
          val fromName = prefixedName(QName(qname.ns), decl, absolutePath = fromPath.nonEmpty)
          irdata.TData(fromPath :+ fromName)
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(_)) =>
          ???
        case decl@ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(_)) =>
          val fromPath = qname.path.map(n => ir.Name(n))
          val fromName = prefixedName(QName(qname.ns), decl, absolutePath = fromPath.nonEmpty)
          irdata.TData(fromPath :+ fromName)
        case ProgramContent.TypeDecl(name, TypeDeclConstraint.SubType(_)) =>
          ???
