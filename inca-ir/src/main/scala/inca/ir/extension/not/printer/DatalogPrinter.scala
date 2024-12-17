package inca.ir.extension.not.printer

import inca.ir.Atom
import inca.ir.extension.not.{Not, WeakNot}
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  override def prettyPrint(atom: Atom): String = atom match
    case Not(at) => s"not(${prettyPrint(at)})"
    case WeakNot(at) => s"weaknot(${prettyPrint(at)})"
    case _ => super.prettyPrint(atom)


