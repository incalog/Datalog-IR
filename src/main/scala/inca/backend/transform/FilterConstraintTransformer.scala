package inca.backend.transform

import inca.backend.ir.IR.{Atom, BodyMustFail}

class FilterConstraintTransformer(predicate: Atom => Boolean) extends Transformer {
  override def transformAtom(atom: Atom): Seq[Atom] =
    if (predicate(atom))
      Seq(atom)
    else
      throw BodyMustFail
}
