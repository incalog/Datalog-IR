package inca.frontend.functional.verification.examples

import inca.compiler.Compiler
import inca.frontend.functional.compiler.FunctionalOptions
import inca.frontend.functional.core._
import inca.frontend.functional.parser.Parser

object Aggregations {

  val integerOperations =
    s"""module IntegerOperations
       |@aggr(assoc, comm) def add(i1: Int, i2: Int): Int = i1 + i2
       |
       |@aggr(assoc, comm) def sub(i1: Int, i2: Int): Int = i1 - i2
       |
       |@aggr(assoc, comm) def mult(i1: Int, i2: Int): Int = i1 * i2
       |
       |@aggr(assoc, comm) def min(i1: Int, i2: Int): Int = if(i1 < i2) i1 else i2
       |
       |@aggr(assoc, comm) def pow(i1: Int, i2: Int): Int = if(i2 <= 0) 1 else if(i2 == 1) i1 else i1 * pow(i1, i2 - 1)
       |""".stripMargin

  val compiledIntegerOperationsModule = Compiler.compileFunctional(integerOperations, FunctionalOptions())

  val doubleOperations =
    s"""module DoubleOperations
       |@aggr(assoc, comm) def add(d1: Double, d2: Double): Double = d1 + d2
       |
       |@aggr(assoc, comm) def sub(d1: Double, d2: Double): Double = d1 - d2
       |
       |@aggr(assoc, comm) def mult(d1: Double, d2: Double): Double = d1 * d2
       |
       |@aggr(assoc, comm) def min(d1: Double, d2: Double): Double = if(d1 < d2) d1 else d2
       |""".stripMargin

  val compiledDoubleOperationsModule = Compiler.compileFunctional(doubleOperations, FunctionalOptions())

  val stringOperations =
    """module StringOperations
      |@aggr(assoc, comm) def concat(s1: String, s2: String): String = s1 + s2
      |""".stripMargin

  val compiledStringOperationsModule = Compiler.compileFunctional(stringOperations, FunctionalOptions())
}

/*
@aggr(assoc, comm) def div(i1: Int, i2: Int): Int = i1 / i2
@aggr(assoc, comm) def div(d1: Double, d2: Double): Double = d1 / d2

*/