package inca.ir.extension.monotypes

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode}


trait Typechecker extends BaseIRTypechecker{
  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case MkMono(m, cls, args, typ) =>
      checkTerm(m, typ, mode)
    case ResultMono(m, output) =>
      val mt = inferTerm(m, mode).ty.asInstanceOf[TMono]
      checkTerm(output, mt.output, mode)
    case AddMono(m, input, keys) =>
      val mt = inferTerm(m, mode).ty.asInstanceOf[TMono]
      checkTerm(input, mt.input, mode)
      for ((key, keyT) <- keys.zip(mt.cols)) {
        checkTerm(key, keyT, mode)
      }
    case _ => super.checkAtom(atom, mode)
}
