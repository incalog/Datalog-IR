package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ModuleEntry, Name, Ref, RefByName, Relation, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst, TypeVar}

trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker:

  def lookupDataDefinition(name: Name, s: SourceLocation): Option[(Seq[Name], DataDefinition)] =
    entries.get(name) match
      case Some(dd: DataDefinition) =>
        Some((Seq(), dd))
      case Some(ParametricModuleEntry(tyParams, dd: DataDefinition)) =>
        Some((tyParams, dd))
      case _ =>
        error(s"Could not find data type $name", s)
        None

  def lookupConstruct(name: Name, locations: SourceLocation*): Option[(Seq[Name], CaseDefinition)] =
    entries.get(name) match
      case Some(cd: CaseDefinition) =>
        Some((Seq(), cd))
      case Some(ParametricModuleEntry(tyParams, cd: CaseDefinition)) =>
        Some((tyParams, cd))
      case _ =>
        error(s"Could not find constructor $name", locations: _*)
        None

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case dd: DataDefinition => // nothing to check
    case CaseDefinition(name, args, data) =>
      checkType(data)
      args.foreach(checkType)
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Construct(ref, args) => lookupConstruct(ref.name, term) match
      case None =>
        error(s"Unknown constructor ${ref.name}", term)
        TAny.bound
      case Some((typeParams, CaseDefinition(_, params, data))) =>
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", term)
        val tyArgs = ref match
          case RefByName(name) =>
            if (typeParams.nonEmpty)
              error(s"Missing type arguments $typeParams for constructor $name", term)
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

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Deconstruct(t, RefByName(name), args, neg) => lookupConstruct(name, atom) match
      case None =>
        error(s"Unknown constructor $name", atom)
      case Some((typeParams, CaseDefinition(_, params, data))) =>
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
      val (tyParams, entry) = lookupDataDefinition(ref.name, ty) match
        case None => (Seq(), null)
        case Some((tyParams, dd)) => (tyParams, dd)
      ref match
        case RefByName(name) =>
          if (tyParams.nonEmpty)
            error(s"Expected type application of $name with ${tyParams.size} type arguments", ty)
        case TypeApplication(name, args) =>
          if (tyParams.size != args.size)
            error(s"Type application has ${args.size} arguments, but $name requires ${tyParams.size} arguments: $entry", ty)
          args.foreach(checkType)
    case _ => super.checkType(ty)