package inca.ir.extension.block

import inca.ir.extension.block.Block
import inca.ir.typing.BaseIRTypechecker
import inca.ir.{Atom, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  override def typecheckInternal(term: Term, hint: Option[Type], bound: Boundedness): Type = term match
    case Block(at, t) =>
      at.foreach(typecheck)
      typecheck(t, hint, bound)
    case _ => super.typecheckInternal(term, hint, bound)