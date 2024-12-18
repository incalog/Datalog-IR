package inca.ir.printer

import inca.ir.extension.*
import inca.ir.{Atom, Body, ModuleEntry, Term}
import inca.util.{Color, TextStyle, colorize, style}

trait IRDebugPrinter extends IRPrinter:

  override val name: String = "IRDebugPrinter"

  override def prettyPrint(moduleEntry: ModuleEntry): String =
    super.prettyPrint(moduleEntry) + moduleEntry.analysisString
      .colorize(Color.Green)
      .style(TextStyle.Bold)

  override def prettyPrint(body: Body): String =
    super.prettyPrint(body) + body.analysisString
      .colorize(Color.Yellow)
      .style(TextStyle.Bold)

  override def prettyPrint(atom: Atom): String =
    super.prettyPrint(atom) + atom.analysisString
      .colorize(Color.Magenta)
      .style(TextStyle.Bold)

  override def prettyPrint(term: Term): String =
    super.prettyPrint(term) + term.analysisString
      .colorize(Color.Blue)
      .style(TextStyle.Bold)
