package inca.frontend.parser

import fastparse.parse 
import fastparse.ScalaWhitespace._
import inca.frontend.core.Core._
import inca.frontend.parser._
import inca.frontend.parser.extensions._

/** Parser Frontend
  * 
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object Parser {
    def parseModule(code : String) : fastparse.Parsed[Module] = {
        parse(code, CoreParser(Seq(
            BoolOpsParser,
            CastParser,
            DataOpCallParser, // not implemented 
            EnumParser,
            ForallExistsParser,
            ForeachParser,
            IfThenElseParser,
            MatchParser,
            SwitchParser
        )).module(_))
    }
}