package inca.ir.extension.primitiveScala

import inca.Scala
import inca.ir.extension.primitiveScala.*
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, TAny, Term, Type}

trait Typechecker extends BaseIRTypechecker:
  override def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match
    case (TScala(scTy1), TScala(scTy2)) => scTy1 == scTy2
    case _ => super.subtype(ty1, ty2)

  override def typecheck(atom: Atom): Unit = atom match
    case Application(out, ty, fun, args) =>
      // TODO: We want to typecheck the Scala code
      // TODO: typecheck fun, get the return value and bind the variable
      args.foreach(typecheck(_, Bound.Assert))
      out.typed(ty)
      typecheck(out, Bound.Assign)
    case _ => super.typecheck(atom)

  override def typecheckInternal(term: Term, inferred: Option[Type], bound: Bound): Type = term match
    case Constant(value, ty) => ty
    case _ => super.typecheckInternal(term, inferred, bound)
