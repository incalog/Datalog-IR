package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ModuleEntry, ModuleImport, ModuleExport, Name, Ref, RefByName, Relation, TAny, Term, TermArg, TermType, Type, Arg, Var, WildcardArg}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst, TypeVar}

trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker:

  private def lookupDataDefinition(ref: Ref[DataDefinitionBase], s: SourceLocation): Option[(Seq[Name], DataDefinitionBase)] =
    lookupModuleEntry(ref.name) match
      case Some(dd: DataDefinition) =>
        ref.resolved(dd)
        Some((Seq(), dd))
      case Some(ddi: DataDefinitionImport) =>
        ref.resolved(ddi)
        Some((Seq(), ddi))
      case Some(ParametricModuleEntry(tyParams, dd: DataDefinition)) =>
        ref.resolved(dd)
        Some((tyParams, dd))
      case Some(ParametricModuleEntry(tyParams, ddi: DataDefinitionImport)) =>
        ref.resolved(ddi)
        Some((tyParams, ddi))
      case _ =>
        error(s"Could not find data type ${ref.name}", s)
        None

  def lookupConstruct(ref: Ref[CaseDefinitionBase], locations: SourceLocation*): Option[(Seq[Name], CaseDefinitionBase)] =
    lookupModuleEntry(ref.name) match
      case Some(cd: CaseDefinition) =>
        ref.resolved(cd)
        Some((Seq(), cd))
      case Some(cdi: CaseDefinitionImport) =>
        ref.resolved(cdi)
        Some(Seq(), cdi)
      case Some(ParametricModuleEntry(tyParams, cd: CaseDefinition)) =>
        ref.resolved(cd)
        Some((tyParams, cd))
      case Some(ParametricModuleEntry(tyParams, cdi: CaseDefinitionImport)) =>
        ref.resolved(cdi)
        Some((tyParams, cdi))
      case _ =>
        error(s"Could not find constructor ${ref.name}", locations: _*)
        None

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case dd: DataDefinition => // nothing to check
    case CaseDefinition(name, args, data) =>
      checkType(data)
      args.foreach(checkType)
    case CaseDefinitionImport(name, args, data) =>
      checkType(data)
      args.foreach(checkType)
    case CaseDefinitionExport(name, args, data) =>
      checkType(data)
      args.foreach(checkType)
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def checkExport(exp: ModuleExport): Unit = exp match
    case dataExport: DataDefinitionExport =>
      lookupModuleEntry(exp.name) match
        case Some(dataDefinition: DataDefinition) => // do nothing
        case _ => error(s"The exported DataDefinition: $exp is not defined")
    case caseExport: CaseDefinitionExport =>
      lookupModuleEntry(exp.name) match
        case Some(caseDefinition: CaseDefinition) => caseExport.args.zip(caseDefinition.args).foreach((t1, t2) => if t1 != t2 then error(s"Type $t1 of export $exp does not match type $t2 of $caseDefinition"))
        case _ => error(s"The exported CaseDefinition: $exp is not defined")
    case _ => super.checkExport(exp)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = 
    def processCase(typeParams: Seq[Name], ref: Ref[CaseDefinitionBase], args: Seq[Term], params: Seq[Type], data: Type): TermType =
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

    term match
      case Construct(ref, args) => lookupConstruct(ref, term) match
        case None =>
          error(s"Unknown constructor ${ref.name}", term)
          TAny.bound
        case Some((typeParams, cd@CaseDefinition(_, params, data))) =>
          addTypeDependency(cd)
          processCase(typeParams, ref, args, params, data)
        case Some((typeParams, cd@CaseDefinitionImport(_, params, data))) =>
          addTypeDependency(cd)
          processCase(typeParams, ref, args, params, data)
      case _ => super.inferTermExtend(term, mode)

  protected override def checkAtom(atom: Atom, mode: Mode): Unit =
    def processCase(t: Term, ref: Ref[CaseDefinitionBase], args: Seq[Arg], neg: Boolean, params: Seq[Type], data: TData): Unit =
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

    atom match
      case Deconstruct(t, ref, args, neg) => lookupConstruct(ref, atom) match
        case None =>
          error(s"Unknown constructor $ref", atom)
        case Some((typeParams, cd@CaseDefinition(_, params, data))) =>
          addTypeDependency(cd)
          processCase(t, ref, args, neg, params, data)
        case Some((typeParams, cd@CaseDefinitionImport(_, params, data))) =>
          addTypeDependency(cd)
          processCase(t, ref, args, neg, params, data)
      case _ => super.checkAtom(atom, mode)

  def checkDeconstruct(matcheeType: Type, dataRef: Ref[DataDefinitionBase], s: SourceLocation): Map[Name, Type] = matcheeType match
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
      val (tyParams, entry) = lookupDataDefinition(ref, ty) match
        case None => (Seq(), null)
        case Some((tyParams, dd)) =>
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

  override def checkImportExport(imp: ModuleImport, exp: ModuleExport): Unit = imp.match
    case dataImp: DataDefinitionImport => exp match
      case dataExp: DataDefinitionExport => // ok
      case _ => error(s"Incompatible Import $imp to Export $exp")
    case caseImp: CaseDefinitionImport => exp match
      case caseExp: CaseDefinitionExport if caseImp.args == caseExp.args => // ok
      case caseExp: CaseDefinitionExport => error(s"Types of $caseImp and $caseExp do not match")
      case _ => error(s"Incompatible Import $imp to Export $exp")
    case _ => super.checkImportExport(imp, exp)
    