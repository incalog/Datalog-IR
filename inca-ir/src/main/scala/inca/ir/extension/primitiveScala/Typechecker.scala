package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.extension.primitiveScala.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TScala(scTy1), TScala(scTy2)) => scTy1 == scTy2
    case _ => super.subtype(ty1, ty2)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Application(out, ty, fun, args) =>
      // TODO: We want to typecheck the Scala code
      // TODO: typecheck fun, get the return value and bind the variable
      args.foreach(inferTerm(_, Mode.Closed))
      checkTerm(out, ty, mode)
    case _ => super.checkAtom(atom, mode)

  override def inferTermExtend(term: Term, mode: Mode): Type = term match
    case Constant(value, ty) => ty
    case _ => super.inferTermExtend(term, mode)
