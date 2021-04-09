package inca.debugger

import inca.frontend.core.tree.{Expression, Name}

import scala.annotation.tailrec

class DebugConsole(debugger: Debugger) {

  /**
   *  s       - step over
   *  si      - step into
   *  so      - step out
   *  l       - print current line
   *  l [n]   - print n lines centered around current line
   *
   *  e | f        - print environment of current stackframe
   *  eall | fall  - print environments of all stackframes in stack
   *
   *  tup [d] - print all tuples of relation with tags up to d depth if specified
   *
   *  q       - quit or exit prompt
   */

  @tailrec
  final def start(): Unit = {
    print(Console.GREEN + ":> " + Console.RESET)
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
            promptForFunction(e.funs, e.resolvedFuns)
          case e: DebugException =>
            println(e.message)
        }
        start()

      case "so" =>
        try {
          debugger.stepOut()
        } catch {
          case e: DebugException => println(e.message)
        }
        start()

      case s"l$n" =>
        val lines = debugger.currentLine(if(n.trim.nonEmpty) n.trim.toInt else 1)
        val curLine = debugger.currentLine().head
        lines.foreach(line => {
          if(line == curLine)
            println(Console.CYAN + curLine + Console.RESET)
          else
            println(line)
        })
        start()

      case "e" | "f" =>
        if(debugger.callStack.nonEmpty)
          println(debugger.callStack.stackFrame)
        start()

      case "eall" | "fall" =>
        println(debugger.callStack)
        start()

      case s"tup$d" =>
        println(debugger.matches.printMatches(if(d.trim.nonEmpty) d.trim.toInt else 0))
        start()

      case "q" =>
      case _ => start()
    }
  }

  @tailrec
  private def promptForBody(): Unit = {
    print(Console.GREEN + ":>> " + Console.RESET)
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
  private def promptForFunction(funs: Seq[(Name, Seq[Expression])], resolvedFuns: Seq[(Name, Seq[Set[EnvValue]])]): Unit = {
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
          debugger.stepIntoFunction(resolvedFuns(f)._1, resolvedFuns(f)._2)
        } catch {
          case e: InvalidCommandException =>
            println(e.message)
            promptForFunction(funs, resolvedFuns)
          case e: MultipleBodiesException =>
            println(e.message)
            promptForBody()
          case _ =>
            promptForFunction(funs, resolvedFuns)
        }
    }
  }
}
