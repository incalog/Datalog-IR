package inca

import inca.CompilationMessage.Severity
import inca.frontend.parser.SourceLocation

case class CompilationMessage(msg: String, sourceLocations: Seq[SourceLocation], severity: Severity)

object CompilationMessage {
  sealed class Severity(val id: Int, override val toString: String)
  object INFO    extends Severity(0, "INFO")
  object WARNING extends Severity(1, "WARNING")
  object ERROR   extends Severity(2, "ERROR")
}