package inca.frontend.parser

import fastparse.parse
import inca.frontend.Frontend
import inca.frontend.core.Core._


object Conversions{
  implicit class IncaParsed(val sc: StringContext) extends AnyVal {
    def mod(args: Any*): Module = {
      val code = buildCode(sc, args)
      parse(code, Frontend.Inca.module(_)).get.value
    }

    def exp(args: Any*): Exp = {
      val code = buildCode(sc, args)
      parse(code, Frontend.Inca.exp(_)).get.value
    }

    def stm(args: Any*): Statement = {
      val code = buildCode(sc, args)
      parse(code, Frontend.Inca.statement(_)).get.value
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