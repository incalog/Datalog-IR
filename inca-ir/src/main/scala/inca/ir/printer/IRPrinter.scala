package inca.ir.printer

import inca.ir.{Atom, Body, ModuleEntry, Term}
import inca.ir.extension.*
import inca.util.{Color, TextStyle, colorize, style}

trait IRPrinter extends BaseIRPrinter
  with aggregate.printer.Printer
  with aggregateset.printer.Printer
  with arithmetic.printer.Printer
  with block.printer.Printer
  with bool.printer.Printer
  with data.printer.Printer
  with datamatch.printer.Printer
  with demand.printer.Printer
  with disjunction.printer.Printer
  with edbdata.printer.Printer
  with foreign.printer.Printer
  with impure.printer.Printer
  with list.printer.Printer
  with map.printer.Printer
  with mono.printer.Printer
  with not.printer.Printer
  with record.printer.Printer
  with set.printer.Printer
  with string.printer.Printer
  with tuple.printer.Printer
  with typeparam.printer.Printer:

  override val name: String = "IRPrinter"