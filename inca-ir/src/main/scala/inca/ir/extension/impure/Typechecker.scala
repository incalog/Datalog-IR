package inca.ir.extension.impure

import inca.ir.Atom
import inca.ir.extension.disjunction.Disjunction
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.string2name

trait Typechecker extends BaseIRTypechecker:
  protected override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case imp@Impure(v, atoms, up, kind) => scopedVariables(Set(v.name)) {
      if (lookupVar(v).isEmpty)
        registerVar(v.name, imp, kind.ty)
      bindVar(v.name)
      atoms.foreach(checkAtom(_, mode))
      checkTerm(up, kind.ty, Mode.Bound)
    }
    case _ => super.checkAtom(atom, mode)