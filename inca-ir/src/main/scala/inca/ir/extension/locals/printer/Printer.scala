package inca.ir.extension.locals.printer

import inca.ir.{Atom, Term, Type}
import inca.ir.printer.BaseIRPrinter
import inca.ir.extension.locals.Assign

trait Printer extends BaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case Assign(v, t) => s"${prettyPrint(v)} = ${prettyPrint(t)}"
    case _ => super.prettyPrint(atom)



