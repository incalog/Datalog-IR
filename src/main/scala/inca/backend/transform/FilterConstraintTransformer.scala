package inca.backend.transform

import inca.backend.ir.GP.Constraint

class FilterConstraintTransformer(predicate: Constraint => Boolean) extends Transformer {
  override def transformConstraint(con: Constraint): Seq[Constraint] =
    if (predicate(con))
      Seq(con)
    else
      Seq()
}
