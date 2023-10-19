package inca.foreign.scala.ir.primitive

import inca.foreign.scala.ir.primitive.*
import inca.foreign.scala.syntax.Scala
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case ScalaTerm(_ , ty, args) =>
      args.foreach(inferTerm(_, Mode.Bound))
      ty.bound
    case _ => super.inferTermExtend(term, mode)
