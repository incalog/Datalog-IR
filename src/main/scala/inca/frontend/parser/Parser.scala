package inca.frontend.parser

import fastparse.ScalaWhitespace._
import fastparse.{End, P, parse}
import inca.frontend.core.Core._
import inca.frontend.parser.extensions._

/** Parser Frontend
  * 
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object Parser {
    private def file_wrapper[_:P]:P[Module] = P(
        CoreParser(Seq(
            BoolOpsParser,
            CastParser,
            DataOpCallParser, // not implemented 
            EnumParser,
            ForallExistsParser,
            ForeachParser,
            IfThenElseParser,
            MatchParser,
            SwitchParser
        )).module ~ End
    )

    def parseModule(code : String) : fastparse.Parsed[Module] = {
        parse(code, file_wrapper(_))
    }
}