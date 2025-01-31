package inca.ir.extension.foreign

import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.*

trait Typechecker extends BaseIRTypechecker:
  override protected def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case ConvertForeignIR(t, fty, irty) =>
      checkTerm(t, fty, mode)
      irty.bound
    case ConvertIRForeign(t, irty, fty) =>
      checkTerm(t, irty, mode)
      fty.bound
    case _ => super.inferTermExtend(term, mode)

  override def checkType(ty: Type): Unit = ty match
    case _ : ForeignType =>
    case _ => super.checkType(ty)
