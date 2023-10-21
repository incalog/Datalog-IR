package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode, TypeErrorException}


trait Typechecker extends BaseIRTypechecker{
  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case MkMono(cls, args, keys) =>
      args.foreach(inferTerm(_, mode))
      TMono(cls.input, cls.output, keys).bound
    case ResultMono(m) =>
      inferTerm(m, mode).ty match
        case TMono(_, output, _) => output.bound
        case t =>
          error(s"Expected type of $m: TMono, actual type of $m: $t")
          t.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case MkMono(cls, args, _) =>
      if (cls.params.length != args.length)
        error(s"The length of input arguments $args is not equal to the length of " +
          s"${cls.name}'s parameters ${cls.params}.")
      else if (args map {inferTerm(_, mode).ty} zip cls.params exists {p => p._1 != p._2})
        error(s"${cls.name}'s parameter list ${cls.params} is not compatible with $args.")
    case AddMono(m, input, keys) =>
      inferTerm(m, mode).ty match
        case TMono(inputTyp, outputTyp, keysTyp) =>
          checkTerm(input, inputTyp, mode)
          if (keys.length != keysTyp.length)
            error(s"$keys is not compatible with the type of $m's keys type $keysTyp")
          keys zip keysTyp foreach {p => checkTerm(p._1, p._2, mode)}
        case t => error(s"Expected type of $m: TMono, actual type of $m: $t")
    case _ => super.checkAtom(atom, mode)
}
