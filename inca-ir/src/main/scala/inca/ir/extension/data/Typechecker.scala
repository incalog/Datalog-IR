package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, Import, ModuleEntry, Name, Providable, Provide, Ref, RefByName, RefByQualifiedName, Relation, Require, Substitution, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst, TypeVar}

import scala.reflect.ClassTag

trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker:

  protected override def checkRequire[T <: ModuleEntry](require: Require): Unit = require match
    case _: RequireDataDefinition => // nothing, we check these on import
    case _: RequireCaseDefinition => // nothing, we check these on import
    case _ => super.checkRequire(require)

  protected override def checkProvide[T <: Providable](provide: Provide[T]): Unit = provide match
    case p: ProvideDataDefinition => inferDataDefinition(p.exportRef, p)
    case p@ProvideCaseDefinition(exportRef, params, dataRef) => inferConstruct(exportRef, p) match
      case Some((_, _, args, data)) =>
        if params.size != args.size then
          error(s"Expected ${params.size} parameters, but got ${args.size}", provide)
        params.zip(args).foreach {
          case (param, ty) => assertComparable(param.ty, ty, provide)
        }
        val expected = inferDataDefinition(dataRef, p)
        if expected != data then
          error(s"Expected $expected, but got $data")
      case _ => // nothing
    case _ => super.checkProvide(provide)

  protected override def checkSubstitution(imp: Import, importable: Substitution[_]): Unit = importable match
    case DataDefinitionSubstitution(to, from) =>
      // Make sure there is a "require" for the "to" name and resolve it
      lookupRequireRef(to, importable, imp.module.target.get)
      inferDataDefinition(from, importable)
    case CaseDefinitionSubstitution(to, toSig, from, fromSig) =>
      if toSig.size != fromSig.size then
        error(s"Expected ${toSig.size} parameters, but got ${fromSig.size}", importable)
      fromSig.zip(toSig).foreach {
        case (fromParam, toParam) => assertComparable(fromParam.ty, toParam.ty, importable)
      }
      lookupRequireRef(to, importable, imp.module.target.get)
      inferConstruct(from, importable)
    case _ => super.checkSubstitution(imp, importable)

  private def inferDataDefinition[D <: ModuleEntry](ref: Ref[D], locations: SourceLocation*)(implicit tag: ClassTag[D]): Option[(Seq[Name], D, Type)] =
    val targetModule = lookupModulePath(ref, locations:_*)
    val res = if targetModule != currentModule then
      // definitions outside the current module must be provided
      // TODO: Handle typeparams in the future?
      val providedData = lookupProvideRef[ProvideDataDefinition](ref, targetModule, locations:_*)
      providedData.map(p => (Seq(), p.asInstanceOf[D], TData(p.exportRef.unqualifiedName)))
    else
      // definitions inside the module can either be a relation or a requirement
      lookupDataDefinition[D](ref, locations:_*)

    res.map { case (_, d, _) => ref.resolved(d) }
    res.map { case (tys, entry, data) => (tys, entry, data) }

  private def lookupDataDefinition[D <: ModuleEntry](ref: Ref[D], s: SourceLocation*)(implicit tag: ClassTag[D]): Option[(Seq[Name], D, Type)] =
    entries.get(ref.name) match
      case Some(dd: DataDefinition) =>
        if (!tag.runtimeClass.isInstance(dd))
          error(s"Expected ${tag.runtimeClass.getSimpleName}, but got ${dd.getClass.getSimpleName} while resolving DataDefinition", s:_*)
        Some((Seq(), dd.asInstanceOf[D], TData(dd.name)))
      case Some(ParametricModuleEntry(tyParams, dd: DataDefinition)) =>
        Some((tyParams, dd.asInstanceOf[D], TData(dd.name)))
      case Some(req: RequireDataDefinition) =>
        Some((Seq(), req.asInstanceOf[D], TData(req.name)))
      case Some(ParametricModuleEntry(tyParams, req: RequireDataDefinition)) =>
        Some((Seq(), req.asInstanceOf[D], TData(req.name)))
      case _ =>
        error(s"Could not find data type ${ref.name}", s:_*)
        None

  def inferConstruct[C <: ModuleEntry](ref: Ref[C], locations: SourceLocation*)(implicit tag: ClassTag[C]): Option[(Seq[Name], C, Seq[Type], TData)] =
    val targetModule = lookupModulePath(ref, locations:_*)
    val cd = if targetModule != currentModule then
      // definitions outside the current module must be provided
      // TODO: Handle typeparams in the future?
      val providedData = lookupProvideRef[ProvideCaseDefinition](ref, targetModule, locations:_*)
      providedData.map(p => (Seq(), p.asInstanceOf[C], p.params.map(_.ty), TData(p.data.unqualifiedName)))
    else
      // definitions inside the module can either be a relation or a requirement
      lookupConstruct[C](ref, locations:_*)

    cd.map { case (_, d, _, _) => ref.resolved(d) }
    cd.map { case (tys, entry, args, data) => (tys, entry, args, data) }

  def lookupConstruct[C <: ModuleEntry](ref: Ref[C], locations: SourceLocation*)(implicit tag: ClassTag[C]): Option[(Seq[Name], C, Seq[Type], TData)] =
    entries.get(ref.name) match
      case Some(cd@CaseDefinition(_, params, data)) =>
        Some((Seq(), cd.asInstanceOf[C], params, data))
      case Some(ParametricModuleEntry(tyParams, cd@CaseDefinition(_, params, data))) =>
        Some((tyParams, cd.asInstanceOf[C], params, data))
      case Some(ParametricModuleEntry(tyParams, req@RequireCaseDefinition(_, params, data))) =>
        Some((tyParams, req.asInstanceOf[C], params.map(_.ty), data))
      case Some(req@RequireCaseDefinition(_, params, data)) =>
        Some((Seq(), req.asInstanceOf[C], params.map(_.ty), data))
      case _ =>
        error(s"Could not find constructor ${ref.name}", locations: _*)
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

  def checkDeconstruct(matcheeType: Type, dataRef: Ref[DataDefinition], s: SourceLocation): Map[Name, Type] = matcheeType match
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
        case TypeApplication(name, args) =>
          if (tyParams.size != args.size)
            error(s"Type application has ${args.size} arguments, but $name requires ${tyParams.size} arguments: $entry", ty)
          args.foreach(checkType)
    case _ => super.checkType(ty)