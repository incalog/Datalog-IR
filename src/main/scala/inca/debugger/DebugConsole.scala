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
   *  pf      - print current function fully, with current line highlighted
   *
   *  e | f        - print environment of current stack frame
   *  eall | fall  - print environments of all stack frames in stack
   *
   *  v (name)     - print bound values in the current stack frame for the given variable name
   *  vall         - print bound values for all variables in the current stack frame
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

      case "pf" =>
        val lines = debugger.currentFun()
        val curLine = debugger.currentLine().head
        lines.foreach(line => {
          if(line.trim == curLine)
            println(Console.CYAN + line + Console.RESET)
          else
            println(line)
        })
        start()

      case "e" | "f" =>
        if(debugger.callStack.nonEmpty && debugger.callStack.isStackFrame())
          debugger.callStack.stackFrame.prettyPrint()
        start()

      case "eall" | "fall" =>
        debugger.callStack.stack.reverse.foreach {
          case frame: StackFrame => frame.prettyPrint()
          case _ =>
        }
        start()

      case "vall" =>
        if(debugger.callStack.nonEmpty && debugger.callStack.isStackFrame())
          println(debugger.callStack.stackFrame)
        start()

      case s"v$n" =>
        if(debugger.callStack.nonEmpty) {
          val name = Name(n.trim)
          if(debugger.callStack.stackFrame.env.contains(name))
            println(s"$name -> ${debugger.callStack.stackFrame.env(name).mkString("{", ", ", "}")}")
          else
            println(s"Unbound var $name in current frame ${debugger.callStack.frame.funName}")
        }
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
