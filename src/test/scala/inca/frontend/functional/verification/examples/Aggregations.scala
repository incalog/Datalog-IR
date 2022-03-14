package inca.frontend.functional.verification.examples

import inca.frontend.functional.core._
import inca.frontend.functional.parser.Parser

object Aggregations {

  val addition =
    s"""module Addition
       |@aggr(assoc, comm) def add(i1: Int, i2: Int): Int = i1 + i2
       |
       |@aggr(assoc, comm) def sub(i1: Int, i2: Int): Int = i1 - i2
       |""".stripMargin

  val addition_module: Module = Parser.parse(addition)

}
