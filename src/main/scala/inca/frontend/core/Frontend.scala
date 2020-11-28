package inca.frontend.core

import inca.frontend.desugar.Desugarable
import inca.frontend.parser.CoreParser
import inca.frontend.typechecker.CoreTypechecker

trait Frontend extends CoreParser with CoreTypechecker {
  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables

  protected def desugarables: Seq[Desugarable] = Seq()

  def parseModule(code: String): fastparse.Parsed[syntax.Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}
