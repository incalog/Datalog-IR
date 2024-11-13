package inca.util

import inca.ir.util.SourceLocation
import inca.util.CompilationMessage.Severity

case class CompilationMessage(msg: String, sourceLocations: Seq[SourceLocation], severity: Severity) {
  override def toString: String =
    s"""$msg
       |  in ${sourceLocations.mkString("\n  in ")}
       |""".stripMargin
}

object CompilationMessage {
  sealed class Severity(val id: Int, override val toString: String)

  object INFO extends Severity(0, "INFO")

  object WARNING extends Severity(1, "WARNING")

  object ERROR extends Severity(2, "ERROR")
}