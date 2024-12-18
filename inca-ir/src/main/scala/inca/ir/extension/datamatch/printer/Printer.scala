package inca.ir.extension.datamatch.printer

import inca.ir.extension.datamatch.{Case, Match}
import inca.ir.{Atom, Term}
import inca.ir.printer.BaseIRPrinter

trait Printer extends BaseIRPrinter:
  def prettyPrint(theCase: Case): String =
    val Case(name, patVars, body) = theCase
    s"case ${prettyPrint(name)}(${patVars.map(prettyPrint).mkString(", ")}) => ${body.map(prettyPrint).mkString(", ")}"

  override def prettyPrint(atom: Atom): String = atom match
    case Match(matchee, cases) => s"${prettyPrint(matchee)} match ${cases.map(prettyPrint).mkString("\n\t\t", "\n\t\t", "\n")}"
    case _ => super.prettyPrint(atom)
