package inca.frontend.util

import java.io.FileNotFoundException

import fastparse.Parsed.{Failure, Success}
import inca.frontend.core.Core
import inca.frontend.parser.Parser
import inca.frontend.typechecker.Typechecker

import scala.collection.mutable
import scala.io.Source

case class Program(modules: Seq[Core.Module]) {
  def prettyprint = {
    modules.map(_.prettyprint("")).mkString("", "\n\n", "")
  }

  def unique: String = {
    val names = modules.map(_.name)
    for (n <- names) {
      if (names.count(_ == n) > 1)
        return n
    }
    ""
  }
}

/**
  * Compiler frontend for the IncA language.
  *
  * @todo    unfinished
  * @version 0.0.1
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object CompilerFrontend {

  val USAGE = s"""|IncAC
                  |Usage:
                  |sbt run file1.inca file2.inca ...
                  |""".stripMargin

  def main(args: Array[String]) {

    if (args.length == 0) {
      println(USAGE)
      sys.exit(0)
    }

    val modules = mutable.ArrayBuffer.empty[Core.Module]
    for (f <- args) {
      try {
        val code = Source.fromFile(f).mkString
        val res = Parser.parseModule(code)
        res match {
          case Failure(label, index, extra) => {
            println(s"Syntax Error: $extra")
            sys.exit()
          }
          case Success(value, index) => modules.addOne(value)
        }
      } catch {
        case e: FileNotFoundException => {
          println(e)
          sys.exit()
        }
        case _: Throwable => { sys.exit(-1) }
      }
    }

    val program = Program(modules.toSeq)
    program.unique match {
      case "" =>
      case m => {
        println(s"Module '$m' is defined multiple times.")
      }
    }

    println(program.prettyprint)
    // println(program)
    println(Typechecker.typecheck(null, program))
  }
}
