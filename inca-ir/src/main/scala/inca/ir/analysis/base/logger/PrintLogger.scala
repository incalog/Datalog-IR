package inca.ir.analysis.base.logger

import sturdy.effect.TrySturdy
import sturdy.fix.{Contextual, Logger}

class PrintLogger[Dom, Codom]
  extends Logger[Dom, Codom]:

  var indent: Int = 0

  def printlnWithIndent(msg: String, enter: Boolean): Unit =
    val indentS = " ".repeat(4).repeat(indent)
    println(s"$indentS$msg")

  override def enter(dom: Dom): Unit =
    printlnWithIndent(s"=> $dom", true)
    indent += 1

  override def exit(dom: Dom, codom: TrySturdy[Codom]): Unit =
    indent -= 1
    printlnWithIndent(s"<= $dom", false)


class ContextualPrintLogger[Dom, Codom, Ctx](using contextual: Contextual[Ctx, Dom, Codom])
  extends PrintLogger[Dom, Codom]:

  override def printlnWithIndent(msg: String, enter: Boolean): Unit =
    val callS = contextual.getCurrentContext
    super.printlnWithIndent(s"$callS$msg", enter)