package inca.frontend

import inca.frontend.core.Core.Module
import inca.frontend.desugar.Desugarable
import inca.frontend.extensions._
import inca.frontend.parser.CoreParser

trait Frontend extends CoreParser {

  final lazy val allDesugarables: Seq[Desugarable] = this.desugarables
  protected def desugarables: Seq[Desugarable] = Seq()

}

object Frontend {
  object Core extends Frontend

  object Inca extends Frontend
    with BoolOpsFrontend
    with CastFrontend
    with DataOpCallFrontentd
    with EnumFrontend
    with ForallExistsFrontend
    with ForeachFrontend
    with IfThenElseFrontend
    with MatchFrontend
    with SwitchFrontend {

    def parseModule(code: String): fastparse.Parsed[Module] = {
      fastparse.parse(code, module(_))
    }
  }
}
