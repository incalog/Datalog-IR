package inca.util

import inca.ir.Module
import inca.ir.printer.{GenericPrinter, IRDebugPrinter}

// Configure this printer to manipulate the way the IR output is printed
lazy val DEFAULT_PRINTER = new IRDebugPrinter {}

def printStep(title: String, content: String): Unit =
  println(title)
  println(content)
  println()
  println("~~~~~~~~~~~~~~~~~~~~~~~")
  println()

def printStep(title: String, content: Module)(implicit printer: GenericPrinter): Unit =
  printStep(title, printer.prettyPrint(content))

def printSteps(title: String, contents: Seq[Module])(implicit printer: GenericPrinter): Unit =
  println(title)
  contents.foreach { content =>
    println(printer.prettyPrint(content))
    println()
    println("~~~~~~~~~~~~~~~~~~~~~~~")
    println()
  }

enum Color(val code: String):
  case Black extends Color("\u001b[30m")
  case Red extends Color("\u001b[31m")
  case Green extends Color("\u001b[32m")
  case Yellow extends Color("\u001b[33m")
  case Blue extends Color("\u001b[34m")
  case Magenta extends Color("\u001b[35m")
  case Cyan extends Color("\u001b[36m")
  case White extends Color("\u001b[37m")
  case Reset extends Color("\u001b[0m")

  override def toString: String = code

enum TextStyle(val code: String):
  case Bold extends TextStyle("\u001b[1m")
  case Dim extends TextStyle("\u001b[2m")
  case Italic extends TextStyle("\u001b[3m")
  case Underline extends TextStyle("\u001b[4m")
  case Blink extends TextStyle("\u001b[5m")
  case Reverse extends TextStyle("\u001b[7m")
  case Hidden extends TextStyle("\u001b[8m")
  case Reset extends TextStyle("\u001b[0m")

  override def toString: String = code

extension (text: String)
  def colorize(color: Color): String = s"$color$text${Color.Reset}"
  def style(style: TextStyle): String = s"$style$text${TextStyle.Reset}"
