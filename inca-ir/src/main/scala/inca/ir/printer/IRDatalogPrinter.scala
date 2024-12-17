package inca.ir.printer

import inca.ir.{Atom, Body, ModuleEntry, Term}
import inca.ir.extension.*
import inca.util.{Color, TextStyle, colorize, style}

trait IRDatalogPrinter extends DatalogBaseIRPrinter
  with aggregate.printer.DatalogPrinter
  with aggregateset.printer.DatalogPrinter
  with arithmetic.printer.DatalogPrinter
  with block.printer.DatalogPrinter
  with bool.printer.DatalogPrinter
  with data.printer.DatalogPrinter
  with datamatch.printer.DatalogPrinter
  with demand.printer.DatalogPrinter
  with disjunction.printer.DatalogPrinter
  with edbdata.printer.DatalogPrinter
  with foreign.printer.DatalogPrinter
  with impure.printer.DatalogPrinter
  with list.printer.DatalogPrinter
  with map.printer.DatalogPrinter
  with mono.printer.DatalogPrinter
  with not.printer.DatalogPrinter
  with record.printer.DatalogPrinter
  with set.printer.DatalogPrinter
  with string.printer.DatalogPrinter
  with tuple.printer.DatalogPrinter
  with typeparam.printer.DatalogPrinter:

  override val name: String = "IRDatalogPrinter"

  override def prettyPrint(moduleEntry: ModuleEntry): String =
    val analysisString =
      if (includeAnalysisString) moduleEntry.analysisString
        .colorize(Color.Green)
        .style(TextStyle.Bold)
      else
        ""
    super.prettyPrint(moduleEntry) + analysisString

  override def prettyPrint(body: Body): String =
    val analysisString =
      if (includeAnalysisString) body.analysisString
        .colorize(Color.Yellow)
        .style(TextStyle.Bold)
      else
        ""
    super.prettyPrint(body) + analysisString

  override def prettyPrint(atom: Atom): String =
    val analysisString =
      if (includeAnalysisString) atom.analysisString
        .colorize(Color.Magenta)
        .style(TextStyle.Bold)
      else
        ""
    super.prettyPrint(atom) + analysisString

  override def prettyPrint(term: Term): String =
    val analysisString =
      if (includeAnalysisString) term.analysisString
        .colorize(Color.Blue)
        .style(TextStyle.Bold)
      else
        ""
    super.prettyPrint(term) + analysisString
