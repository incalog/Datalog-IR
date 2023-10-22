package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}


trait Typechecker extends BaseIRTypechecker{
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MkMono(mono, args, keyTypes) =>
      val (in, out) = mono.typecheck(args) match
        case Left(msg) =>
          error(msg, term)
          (TAny, TAny)
        case Right(tm) => tm
      TMono(in, out, keyTypes).bound
    case ResultMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(_, output, _) => output.bound
        case t =>
          error(s"Expected type of $m: TMono, actual type of $m: $t")
          TAny.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case AddMono(m, input, key) =>
      inferTerm(m, mode).ty match
        case TMono(inType, outType, keyType) =>
          checkTerm(input, inType, Mode.Bound)
          checkTerm(key, keyType, Mode.Bound)
        case ty => error(s"Expected $m to have Mono type, but was $ty", m)
    case _ => super.checkAtom(atom, mode)
}
