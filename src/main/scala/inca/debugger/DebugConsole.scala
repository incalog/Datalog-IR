package inca.debugger

import inca.frontend.core.tree.{Expression, Name}

import scala.annotation.tailrec

class DebugConsole(debugger: Debugger) {
  @tailrec
  final def start(): Unit = {
    Console.in.readLine() match {
      case "s" =>
        try {
          debugger.stepOver()
        } catch {
          case e: MultipleBodiesException =>
            println(e.message)
            promptForBody()
          case e: DebugException =>
            println(e.message)
        }
        start()

      case "si" =>
        try {
          debugger.stepInto()
        } catch {
          case e: MultipleBodiesException =>
            println(e.message)
            promptForBody()
          case e: MultipleFunctionCallsException =>
            println(e.message)
            promptForFunction(e.funs)
          case e: DebugException =>
            println(e.message)
        }
        start()

      case "so" =>
        debugger.stepOut()
        start()

      case s"print$d" =>
        println(debugger.matches.printMatches(if(d.nonEmpty) d.trim.toInt else 0))
        start()

      case s"p" =>
        println("Env:\n" + debugger.callStack)
        start()

      case s"l" =>
        println(":> " + debugger.currentLine)
        start()

      case s"dd" =>
        println("Intermediary vals " + debugger.callStack.frame().intermVars)
        start()

      case "q" =>
      case _ => start()
    }
  }

  @tailrec
  private def promptForBody(): Unit = {
    print(":>> ")
    Console.in.readLine() match {
      case "q" =>
      case s"$i" =>
        try {
          debugger.stepIntoBody(i.trim.toInt)
        } catch {
          case _: NumberFormatException =>
            println(s"Invalid index $i")
            promptForBody()
          case e: InvalidCommandException =>
            println(e.message)
            promptForBody()
        }
    }
  }

  @tailrec
  private def promptForFunction(funs: Seq[(Name, Seq[Expression])]): Unit = {
    val fs = funs.indices.zip(funs)
    println(fs.map {
      case (i, f) => s"$i: ${f._1}(${f._2.mkString(", ")})"
    }.mkString("\n"))

    print(":>> ")
    Console.in.readLine() match {
      case "q" =>
      case s"$i" =>
        try {
          val f = i.trim.toInt
          debugger.stepIntoFunction(funs(f)._1, funs(f)._2)
        } catch {
          case e: InvalidCommandException =>
            println(e.message)
            promptForFunction(funs)
          case e: MultipleBodiesException =>
            println(e.message)
            promptForBody()
          case _ =>
            promptForFunction(funs)
        }
    }
  }
}
