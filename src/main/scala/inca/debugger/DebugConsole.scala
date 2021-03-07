package inca.debugger

import scala.annotation.tailrec

class DebugConsole(debugger: Debugger) {
  @tailrec
  final def start(): Unit = {
    Console.in.readLine() match {
      case "s" =>
        debugger.stepOver()
        start()

      case "si" =>
        debugger.stepInto()
        start()

      case "so" =>
        debugger.stepOut()
        start()

      case s"print$d" =>
        println(debugger.matches.printMatches(if(d.nonEmpty) d.trim.toInt else 0))
        start()

      case s"p" =>
        println("Env:\n" + debugger.debugEnv.env.mkString("\n"))
        start()

      case "q" =>
      case _ => start()
    }
  }
}
