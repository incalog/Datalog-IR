package inca.ir.extension.bool

import inca.ir.extension.bool.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TBoolean, TBoolean) => true
    case (_, TBoolean) => false
    case (TBoolean, _) => false
    case _ => super.subtype(ty1, ty2)

  override def typecheck(atom: Atom): Unit = atom match
    case BoolAtom(t) => typecheck(t, Bound.Assert)
    case _ => super.typecheck(atom)

  override def typecheckInternal(term: Term, inferred: Option[Type], bound: Bound): Type = term match
    case AtomAsBool(a: Atom) => TBoolean
    case BoolAnd(t1, t2) => (typecheck(t1, Bound.Assert), typecheck(t2, Bound.Assert)) match
      case (TBoolean, TBoolean) => TBoolean
      case (ty1, ty2) =>
        error(s"Expected booleans, but got $ty1, $ty2", term)
        TAny
    case BoolOr(t1, t2) => (typecheck(t1, Bound.Assert), typecheck(t2, Bound.Assert)) match
      case (TBoolean, TBoolean) => TBoolean
      case (ty1, ty2) =>
        error(s"Expected booleans, but got $ty1, $ty2", term)
        TAny
    case BoolNot(t) => typecheck(t, Bound.Assert) match
      case TBoolean => TBoolean
      case ty =>
        error(s"Expected a boolean, but got $ty", term)
        TAny
    case BoolFalse => TBoolean
    case BoolTrue => TBoolean
    case _ => super.typecheckInternal(term, inferred, bound)
