package inca.ir.extension.disjunction.printer

import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.Atom
import inca.ir.printer.DatalogBaseIRPrinter

trait DatalogPrinter extends DatalogBaseIRPrinter:
  def prettyPrint(disjunctionAlternative: DisjunctionAlternative): String =
    disjunctionAlternative.body.atoms.map(prettyPrint).mkString("{", ", ", "}")

  override def prettyPrint(atom: Atom): String = atom match
    case Disjunction(alternatives) => alternatives.map(prettyPrint).mkString(" or ")
    case _ => super.prettyPrint(atom)
