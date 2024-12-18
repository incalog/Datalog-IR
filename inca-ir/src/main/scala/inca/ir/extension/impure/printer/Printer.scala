package inca.ir.extension.impure.printer

import inca.ir.Atom
import inca.ir.extension.impure.{ImpurityKind, Impure}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  def prettyPrint(impurityKind: ImpurityKind): String = impurityKind.name

  override def prettyPrint(atom: Atom): String = atom match
    case Impure(v, atoms, update, kind) => s"Impure(${prettyPrint(v)} => ${atoms.map(prettyPrint).mkString(", ")}, ${prettyPrint(update)})"
    case _ => super.prettyPrint(atom)
