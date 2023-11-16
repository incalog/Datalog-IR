package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, ModuleEntry, Name, Relation, TAny, Term, TermType, Type, Var}


trait Typechecker extends BaseIRTypechecker with TypeContext:
  override def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case d: DataDefinition => bindData(d)
    case _ => super.typecheck(moduleEntry)

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
    case Deconstruct(t, name, args) => lookupConstruct(name, atom) match
      case None =>
        error(s"Unknown constructor $name", atom)
      case Some((DataDefinition(dataName, _), CaseDefinition(_, params))) =>
        checkTerm(t, TData(dataName), Mode.Bound)
        if (args.size != params.size)
          error(s"Expected ${params.size} arguments but got: ${args.size}", atom)
        args.zip(params).foreach { case (v, ty) =>
          checkTerm(v, ty, mode)
        }
    case NegDeconstruct(t, name) => lookupConstruct(name, atom) match
      case None =>
        error(s"Unknown constructor $name", atom)
      case Some((DataDefinition(dataName, _), CaseDefinition(_, params))) =>
        checkTerm(t, TData(dataName), Mode.Bound)
    case _ => super.checkAtom(atom, mode)
