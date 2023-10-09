package inca.ir.extension.block

import inca.ir.extension.block.Block
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  protected override def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode = term match
    case Block(at, t) =>
      at.foreach(checkAtom(_, mode))
      checkTerm(t, expected, mode)
    case _ => super.checkTermExtend(term, expected, mode)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case Block(at, t) =>
      at.foreach(checkAtom(_, mode))
      inferTerm(t, mode)
    case _ => super.inferTermExtend(term, mode)