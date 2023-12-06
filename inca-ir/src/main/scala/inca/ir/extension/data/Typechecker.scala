package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{Atom, ModuleEntry, Name, Ref, RefByName, Relation, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeSubst}

trait Typechecker extends BaseIRTypechecker with typeparam.Typechecker with TypeContext:
  override def bindModuleEntry(entry: ModuleEntry): Unit = entry match
    case d: DataDefinition =>
      super.bindModuleEntry(entry)
      bindData(Seq(), d)
    case ParametricModuleEntry(typeParams, d: DataDefinition) =>
      super.bindModuleEntry(entry)
      bindData(typeParams, d)
    case _ => super.bindModuleEntry(entry)

  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case d: DataDefinition => d.cases.foreach(_.args.foreach(checkType))
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Construct(ref, args) => lookupConstruct(ref.name, term) match
      case None =>
        error(s"Unknown constructor ${ref.name}", term)
        TAny.bound
      case Some((typeParams, DataDefinition(dataName, _), CaseDefinition(_, params))) =>
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
        TData(TypeApplication.make(dataName, tyArgs)).bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Deconstruct(t, RefByName(name), args, neg) => lookupConstruct(name, atom) match
      case None =>
        error(s"Unknown constructor $name", atom)
      case Some((typeParams, DataDefinition(dataName, _), CaseDefinition(_, params))) =>
        ???
        checkTerm(t, TData(dataName), Mode.Bound)
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", atom)
        val argMode = if (neg) Mode.Collapse else mode
        args.zip(params).foreach {
          case (TermArg(v), ty) => checkTerm(v, ty, mode)
          case (wildcard@WildcardArg(), ty) => wildcard.typed(ty.collapsed, force = true)
        }
    case _ => super.checkAtom(atom, mode)

  override def checkType(ty: Type): Unit = ty match
    case TData(ref@RefByName(name)) => lookupModuleEntry(name) match
      case None => error(s"Unknown data type $name", ty)
      case Some(dd@DataDefinition(`name`, _)) => ref.resolved(dd) // good
      case Some(entry) => error(s"Expected data type definition $name but found $entry", ty)
    case TData(ref@TypeApplication(name, args)) => lookupModuleEntry(name) match
      case None => error(s"Unknown data type $name", ty)
      case Some(entry@ParametricModuleEntry(tyParams, dd@DataDefinition(`name`, _))) =>
        ref.resolved(dd)
        if (tyParams.size != args.size)
          error(s"Type application has ${args.size} arguments, but $name requires ${tyParams.size} arguments: $entry", ty)
        args.foreach(checkType)
      case Some(entry) => error(s"Expected polymorphic data type $name but found $entry", ty)
    case _ => super.checkType(ty)