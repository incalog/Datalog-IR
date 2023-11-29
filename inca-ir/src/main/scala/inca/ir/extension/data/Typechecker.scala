package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, ModuleEntry, Name, Relation, TAny, Term, TermArg, TermType, Type, Var, WildcardArg}


trait Typechecker extends BaseIRTypechecker with TypeContext:
  override def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case d: DataDefinition => bindData(d)
    case _ => super.checkModuleEntry(moduleEntry)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Construct(name, args) => lookupConstruct(name, term) match
      case None =>
        error(s"Unknown constructor $name", term)
        TAny.bound
      case Some((DataDefinition(dataName, _), CaseDefinition(_, params))) =>
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", term)
        args.zip(params).foreach { case (t, ty) =>
          checkTerm(t, ty, Mode.Bound)
        }
        TData(dataName).bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Deconstruct(t, name, args, neg) => lookupConstruct(name, atom) match
      case None =>
        error(s"Unknown constructor $name", atom)
      case Some((DataDefinition(dataName, _), CaseDefinition(_, params))) =>
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
    case TData(_) => // good
    case _ => super.checkType(ty)