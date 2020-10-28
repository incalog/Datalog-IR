//package inca.frontend.util
//
//import java.io.FileNotFoundException
//
//import fastparse.Parsed.{Failure, Success}
//import inca.frontend.Frontend
//import inca.frontend.core.Core
//import inca.runtime.context.LanguageMetaInfo
//
//import scala.collection.mutable
//import scala.io.Source
//
//case class Program(modules: Seq[Core.Module]) {
//  def prettyprint: String = {
//    modules.map(_.prettyprint("")).mkString("", "\n\n", "")
//  }
//
//  def unique: String = {
//    val names = modules.map(_.name)
//    for (n <- names) {
//      if (names.count(_ == n) > 1)
//        return n.name
//    }
//    ""
//  }
//}
//
///**
//  * Compiler frontend for the IncA language.
//  *
//  * @todo    unfinished
//  * @version 0.0.1
//  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
//  *          Julian Cichorius (jcichori@students.uni-mainz.de)
//  */
//object CompilerFrontend {
//
//  val USAGE: String = s"""|Usage:
//                          |sbt run file1.inca file2.inca ...
//                          |""".stripMargin
//
//  def main(args: Array[String]) {
//    val emptyLMI = new LanguageMetaInfo() // @todo
//
//    println("\nIncAC\n")
//    if (args.length == 0) {
//      println(USAGE)
//      sys.exit(0)
//    } else {
//      println(s"> Processing files: ${args.mkString(", ")}")
//    }
//
//    val modules = mutable.ArrayBuffer.empty[Core.Module]
//    for (f <- args) {
//      var file: Source = null
//      try {
//        file = Source.fromFile(f)
//        val code = file.mkString
//        val res = Frontend.Inca(emptyLMI).parseModule(code)
//        res match {
//          case Failure(_, _, extra) =>
//            println(s"> Syntax error in file '$f':\n'''")
//
//            val index = extra.index
//
//            var count = 0
//            for (l <- code.split("\n")) {
//              if (l.length() + count < index || count < 0) {
//                println(l)
//                count += l.length() + 1
//              } else {
//                println(l)
//                val buffer = new StringBuilder
//                for (i <- 0 until (index - count - 1))
//                  buffer.append(" ")
//                println(buffer + "^ arround here !!!")
//                count = -1
//              }
//            }
//            println("'''")
//            sys.exit()
//          case Success(value, index) => modules.addOne(value)
//        }
//      } catch {
//        case e: FileNotFoundException =>
//          println(e)
//          sys.exit()
//        case _: Throwable => sys.exit(-1)
//      } finally {
//        if (file != null)
//          file.close()
//      }
//    }
//
//    val program = Program(modules.toSeq)
//    program.unique match {
//      case "" =>
//      case m =>
//        println(s"> Module '$m' is defined multiple times.")
//        sys.exit()
//    }
//
//    println("> Parsed Code:\n'''\n" + program.prettyprint + "\n'''")
//    Typechecker.typecheck(emptyLMI, program) match {
//      case FailTypecheck(errors, warnings) =>
//        println(
//          "> Typecheck failed.\n> Errors found:\n" + errors
//            .mkString("\n") + "\n" + (if (warnings.nonEmpty)
//                                        s"> Warnings found:\n${warnings.mkString("\n")}"
//                                      else "")
//        )
//      case SuccessTypecheck(warnings) =>
//        println(
//          "> Typecheck succeeded.\n" + (if (warnings.nonEmpty)
//                                          s"> Warnings found:\n${warnings.mkString("\n")}"
//                                        else "")
//        )
//    }
//  }
//}
