package inca.ir.extension.locals

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.extension.locals.*

trait Typechecker extends BaseIRTypechecker:
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Assign(v@Var(ref), t) if isParam(ref.name) =>
      error(s"Can not reassign parameter ${ref.name}", v, atom)
    case Assign(v@Var(ref), t) =>
      val ty = inferTerm(t, Mode.Bound).ty
      lookupVar(ref) match
        case Some(varInfo) =>
          assertComparable(varInfo.ty, ty, atom)
        case None =>
          error(s"Can not assign unbound variable $v", v, atom)
    case _ => super.checkAtom(atom, mode)