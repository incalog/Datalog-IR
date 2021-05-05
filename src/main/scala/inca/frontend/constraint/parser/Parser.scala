package inca.frontend.constraint.parser

import inca.frontend.constraint.extensions
import inca.frontend.constraint.core.tree.Module

object Parser {
  private lazy val parser: CoreParser = new CoreParser with
    extensions.boolOps.Parser with
    extensions.evalCall.Parser with
    extensions.forallExists.Parser with
    extensions.foreach.Parser with
    extensions.ifThenElse.Parser with
    extensions.match_.Parser with
    extensions.switch_.Parser {}

  def parse(code: String): Module = {
    import fastparse.Parsed

    fastparse.parse(code, parser.module(_), verboseFailures = true) match {
      case Parsed.Success(value, _) => value
      case fail: Parsed.Failure =>
        throw new IllegalArgumentException(s"Parsing Error: ${fail.trace(true).longTerminalsMsg}")
    }
  }
}
