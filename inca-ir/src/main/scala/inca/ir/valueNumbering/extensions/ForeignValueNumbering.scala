package inca.ir.valueNumbering.extensions

import inca.ir.Term
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign, ForeignTerm, ForeignType}
import inca.ir.valueNumbering.BaseVN.BaseValueNumbering


trait ForeignValueNumbering extends BaseValueNumbering{

  override def isAllowedToReplace(term: Term): Boolean = term match {
    case ConvertForeignIR(_, _, _) => false
    case ConvertIRForeign(_, _, _) => false
    case _: ForeignTerm => false
    case _ => super.isAllowedToReplace(term)
  }

}
