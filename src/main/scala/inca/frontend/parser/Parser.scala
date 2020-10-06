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

  val parser: CoreParser = CoreParser(Seq(
    BoolOpsParser,
    CastParser,
    DataOpCallParser, // not implemented
    EnumParser,
    ForallExistsParser,
    ForeachParser,
    IfThenElseParser,
    MatchParser,
    SwitchParser
  ))

  def parseModule(code : String) : fastparse.Parsed[Module] = {
      parse(code, parser.module(_))
  }
}

object Conversions{
  implicit class IncaParsed(val sc: StringContext) extends AnyVal {
    def mod(args: Any*): Module = {
      val code = buildCode(sc, args)
      Parser.parseModule(code).get.value
    }

    def exp(args: Any*): Exp = {
      val code = buildCode(sc, args)
      parse(code, Parser.parser.exp(_)).get.value
    }

    def stm(args: Any*): Statement = {
      val code = buildCode(sc, args)
      parse(code, Parser.parser.statement(_)).get.value
    }
  }

  private def buildCode(sc: StringContext, args: Any*): String = {
    val argStrings = args.map {
        case str: String => str
        case exp: Exp => exp.prettyprint("")
        case stm: Statement => stm.prettyprint("")
        case o => s"${o.toString}"
    }
    val sb = new StringBuilder()
    val (init, last) = (sc.parts.init, sc.parts.last)
    init.zip(argStrings).foreach {
        case (lit, arg) => sb.append(lit.appendedAll(arg))
    }
    sb.append(last).toString()
  }
}