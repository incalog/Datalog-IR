package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, Import, ModuleEntry, Name, Provide, Ref, RefByName, RefByQualifiedName, Relation, Require, Substitution, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst, TypeVar}

trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker:

  protected override def checkRequire[T <: ModuleEntry](require: Require): Unit = require match
    case _: RequireDataDefinition => // nothing, we check these on import
    case _: RequireCaseDefinition => // nothing, we check these on import
    case _ => super.checkRequire(require)

  protected override def checkProvide[T <: ModuleEntry](provide: Provide[T]): Unit = provide match
    case p@ProvideDataDefinition(exportRef) => inferDataDefinition(exportRef, p)
    case p@ProvideCaseDefinition(exportRef, pArgs, dd) => inferConstruct(exportRef, p) match
      case Some((_, _, args, data)) =>
        if pArgs.size != args.size then
          error(s"Expected ${pArgs.size} parameters, but got ${args.size}", provide)
        pArgs.zip(args).foreach((aTy, ty) => assertComparable(aTy, ty, provide))
        inferDataDefinition(dd.ref, p) match
          case Some(_, _, ty) if ty == data => // nothing
          case Some(_, _, ty) => error(s"Expected $data, but got $ty")
          case _ => error(s"Could not infer data definition ${dd.ref}")
      case _ => // nothing
    case _ => super.checkProvide(provide)

  protected override def checkSubstitution[T <: Require](imp: Import, importable: Substitution[T, ?]): Unit = importable match
    case DataDefinitionSubstitution(to, from) =>
      // Make sure there is a "require" for the "to" name and resolve it
      lookupRequireRef(to, importable, imp.module.target.get)
      inferDataDefinition(from, importable)
    case CaseDefinitionSubstitution(to, toSig, from, fromSig) =>
      if toSig.size != fromSig.size then
        error(s"Expected ${toSig.size} parameters, but got ${fromSig.size}", importable)
      fromSig.zip(toSig).foreach {
        case (fromArg, toArg) => assertComparable(fromArg, toArg, importable)
      }
      lookupRequireRef(to, importable, imp.module.target.get)
      inferConstruct(from, importable)
    case _ => super.checkSubstitution(imp, importable)

  private def inferDataDefinition[D <: ModuleEntry](ref: Ref[D], locations: SourceLocation*): Option[(Seq[Name], D, Type)] =
    val targetModule = lookupModuleByPath(ref, locations*)
    val res = if targetModule != currentModule then
      // definitions outside the current module must be provided
      // TODO: Handle typeparams in the future
      val providedData = lookupProvideRef[ProvideDataDefinition](ref, targetModule, locations*)
      providedData.map(p => (Seq(), p.asInstanceOf[D], TData(ref.path :+ p.exportRef.unqualifiedName)))
    else
      // definitions inside the module can either be a relation or a requirement
      lookupDataDefinition[D](ref, locations*)

    res.map { case (_, dd, _) => ref.resolved(dd) }
    res.map { case (tys, entry, data) => (tys, entry, data) }

  private def lookupDataDefinition[D <: ModuleEntry](ref: Ref[D], s: SourceLocation*): Option[(Seq[Name], D, Type)] =
    entries.get(ref.name) match
      case Some(dd: DataDefinition) =>
        Some((Seq(), dd.asInstanceOf[D], TData(dd.name)))
      case Some(ParametricModuleEntry(tyParams, dd: DataDefinition)) =>
        Some((tyParams, dd.asInstanceOf[D], TData(dd.name)))
      case Some(req: RequireDataDefinition) =>
        Some((Seq(), req.asInstanceOf[D], TData(req.name)))
      case Some(ParametricModuleEntry(tyParams, req: RequireDataDefinition)) =>
        Some((Seq(), req.asInstanceOf[D], TData(req.name)))
      case _ =>
        error(s"Could not find data type ${ref.name}", s*)
        None

  def inferConstruct[C <: ModuleEntry](ref: Ref[C], locations: SourceLocation*): Option[(Seq[Name], C, Seq[Type], TData)] =
    val targetModule = lookupModuleByPath(ref, locations*)
    val res = if targetModule != currentModule then
      // definitions outside the current module must be provided
      // TODO: Handle typeparams in the future
      val providedData = lookupProvideRef[ProvideCaseDefinition](ref, targetModule, locations*)
      providedData.map { p =>
        // add the path suffix to all TData types
        val qualifiedDataTypes = p.args.map {
          case TData(dRef) => TData(ref.path ++ dRef.path :+ dRef.unqualifiedName)
          case ty => ty
        }
        (Seq(), p.asInstanceOf[C], qualifiedDataTypes, TData(ref.path :+ p.data.ref.unqualifiedName))
      }
    else
      // definitions inside the module can either be a relation or a requirement
      lookupConstruct[C](ref, locations*)

    res.map { case (_, cd, _, _) => ref.resolved(cd) }
    res.map { case (tys, entry, args, data) => (tys, entry, args, data) }

  def lookupConstruct[C <: ModuleEntry](ref: Ref[C], locations: SourceLocation*): Option[(Seq[Name], C, Seq[Type], TData)] =
    entries.get(ref.name) match
      case Some(cd@CaseDefinition(_, params, data)) =>
        Some((Seq(), cd.asInstanceOf[C], params, data))
      case Some(ParametricModuleEntry(tyParams, cd@CaseDefinition(_, params, data))) =>
        Some((tyParams, cd.asInstanceOf[C], params, data))
      case Some(ParametricModuleEntry(tyParams, req@RequireCaseDefinition(_, args, data))) =>
        Some((tyParams, req.asInstanceOf[C], args, data))
      case Some(req@RequireCaseDefinition(_, args, data)) =>
        Some((Seq(), req.asInstanceOf[C], args, data))
      case _ =>
        error(s"Could not find constructor ${ref.name}", locations*)
        None

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case dd: DataDefinition => // nothing to check
    case CaseDefinition(name, args, data) =>
      checkType(data)
      args.foreach(checkType)
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Construct(ref, args) => inferConstruct(ref, term) match
      case None =>
        error(s"Unknown constructor ${ref.name}", term)
        TAny.bound
      case Some((typeParams, cd, params, data)) =>
        addTypeDependency(cd)
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", term)
        val tyArgs = ref match
          case RefByName(name) =>
            if (typeParams.nonEmpty)
              error(s"Missing type arguments $typeParams for constructor $name", term)
            Seq()
          case ref@RefByQualifiedName(_) =>
            if (typeParams.nonEmpty)
              error(s"Missing type arguments $typeParams for constructor ${ref.name}", term)
            Seq()
          case TypeApplication(name, tyArgs) =>
            if (typeParams.size != tyArgs.size)
              error(s"Wrong number of type arguments, got ${tyArgs.size} but expected ${typeParams.size} for constructor $name", term)
            tyArgs
        val tySubst = new TypeSubst(typeParams.zip(tyArgs).toMap)
        args.zip(params).foreach { case (t, ty) =>
          if (tySubst.subst.isEmpty)
            checkTerm(t, ty, Mode.Bound)
          else {
            val tyInst = tySubst.visitType(ty)
            checkTerm(t, tyInst, Mode.Bound)
          }
        }
        val resultType = tySubst.visitType(data)
        resultType.bound
    case _ => super.inferTermExtend(term, mode)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Deconstruct(t, ref, args, neg) => inferConstruct(ref, atom) match
      case None =>
        error(s"Unknown constructor $ref", atom)
      case Some((typeParams, cd, params, data)) =>
        addTypeDependency(cd)
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", atom)

        val ty = inferTerm(t, Mode.Bound).ty
        val substMap = checkDeconstruct(ty, data.ref, t)
        val subst = new TypeSubst(substMap)
        val substedParams = params.map(subst.visitType)
        args.zip(substedParams).foreach {
          case (TermArg(v), ty) => checkTerm(v, ty, mode)
          case (wildcard@WildcardArg(), ty) => wildcard.typed(ty.collapsed, force = true)
        }
    case _ => super.checkAtom(atom, mode)

  def checkDeconstruct(matcheeType: Type, dataRef: Ref[DataDefinitionReference], s: SourceLocation): Map[Name, Type] = matcheeType match
    case TData(matcheeRef) =>
      if (matcheeRef.name != dataRef.name)
        error(s"Constructor ${dataRef.name} does not belong to matchee's data type ${matcheeRef.name}", matcheeRef)

      (matcheeRef, dataRef) match
        case (t1@TypeApplication(_, matcheeTypeArgs), TypeApplication(_, caseTypeArgs)) =>
          caseTypeArgs.zip(matcheeTypeArgs).flatMap {
            case (TypeVar(x), ty) => Some(x -> ty)
            case (ty1, ty2) =>
              if (ty1 != ty2)
                error(s"Cannot match type argument $ty1 (from case type $dataRef) against " +
                      s"type argument $ty2 (from matchee type $matcheeRef)", s)
              None
          }.toMap
        case _ =>
          // either this is fine, or one of the types is ill-kinded anyways, and an error has already been raised
          Map()
    case ty =>
      error(s"Expected data type but got $matcheeType", s)
      Map()

  override def checkType(ty: Type): Unit = ty match
    case TData(ref) =>
      val (tyParams, entry) = inferDataDefinition(ref, ty) match
        case None => (Seq(), null)
        case Some((tyParams, dd, _)) =>
          addTypeDependency(dd)
          (tyParams, dd)
      ref match
        case RefByName(name) =>
          if (tyParams.nonEmpty)
            error(s"Expected type application of $name with ${tyParams.size} type arguments", ty)
        case RefByQualifiedName(names) =>
          if (tyParams.nonEmpty)
            error(s"Expected type application of ${ref.name} with ${tyParams.size} type arguments", ty)
        case TypeApplication(name, args) =>
          if (tyParams.size != args.size)
            error(s"Type application has ${args.size} arguments, but $name requires ${tyParams.size} arguments: $entry", ty)
          args.foreach(checkType)
    case _ => super.checkType(ty)