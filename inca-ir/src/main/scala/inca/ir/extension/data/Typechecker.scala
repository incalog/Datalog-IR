package inca.ir.extension.data

import inca.ir.extension.data.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, ModuleEntry, Relation, TAny, Term, Type, Var}


trait Typechecker extends BaseIRTypechecker with TypeContext:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TData(name1), TData(name2)) => name1 == name2
    case (TData(_), _) => false
    case (_, TData(_)) => false
    case _ => super.subtype(ty1, ty2)

  override def typecheck(moduleEntry: ModuleEntry): Unit = moduleEntry match
    case d: DataDefinition => bindData(d)
    case _ => super.typecheck(moduleEntry)

  override def typecheck(atom: Atom): Unit = atom match
    case Match(matchee, cases) =>
      typecheck(matchee, Bound.Assert)
      cases.foreach {
        case Case(name, vars, body) =>
          val argTys = lookupConstruct(name, atom) match
            case Some((_, CaseDefinition(name, tys))) => tys
            case None => Seq()

          if (argTys.size != vars.size)
            error(s"Expected ${argTys.size} arguments, but got ${vars.size}", atom)

          vars.zipWithIndex.foreach {
            case (v@Var(name), idx) =>
              val expectedArgTy = argTys(idx)
              // Make sure new variables get bound
              registerVar(name, v, expectedArgTy)
              assertSubtype(typecheck(v, Bound.Assign), expectedArgTy)
          }
          body.foreach(typecheck)
      }
    case _ => super.typecheck(atom)

  override def typecheckInternal(term: Term, inferred: Option[Type], bound: Bound): Type = term match
    case Construct(name, data) => lookupConstruct(name, term) match
      case Some((DataDefinition(dataName, _), _)) => TData(dataName)
      case None => TAny
    case _ => super.typecheckInternal(term, inferred, bound)
