package inca.frontend_old.core

import inca.frontend_old.desugar.Desugarable
import inca.frontend_old.parser.CoreParser
import inca.frontend_old.typechecker.CoreTypechecker

trait Frontend extends CoreParser with CoreTypechecker {
  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables

  protected def desugarables: Seq[Desugarable] = Seq()

  def parseModule(code: String): fastparse.Parsed[syntax.Module] = {
    fastparse.parse(code, module(_), verboseFailures = true)
  }
}
