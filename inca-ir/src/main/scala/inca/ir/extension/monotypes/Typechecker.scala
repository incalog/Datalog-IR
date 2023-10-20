package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}


trait Typechecker extends BaseIRTypechecker{
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MkMono(cls, args, monoTyp) =>
      args.foreach(inferTerm(_, mode))
      monoTyp.bound
    case ResultMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(_, output, _) => output.bound
        case t =>
          error(s"Expected type of $m: TMono, actual type of $m: $t")
          t.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case AddMono(m, input, keys) =>
      inferTerm(m, mode).ty match
        case mt@TMono(inputTyp, outputTyp, keysTyp) =>
          checkTerm(input, inputTyp, mode)
          if (keys.length != mt.keys.length)
            error(s"$keys is not compatible with the type of $m's keys type ${mt.keys}")
          for ((key, typ) <- keys.zip(mt.keys)) {
            checkTerm(key, typ, mode)
          }
        case t => error(s"Expected type of $m: TMono, actual type of $m: $t")
    case _ => super.checkAtom(atom, mode)
}
