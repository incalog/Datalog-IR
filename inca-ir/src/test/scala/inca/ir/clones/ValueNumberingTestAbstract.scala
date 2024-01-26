package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.analysis.{ConfigVN, ValueNumbering}



abstract class ValueNumberingTestAbstract extends AnyFunSuite{

  val config: ConfigVN = ConfigVN()

  def performTest(expected: IRModule, input: IRModule, config: ConfigVN = config): Unit = {
    val VN = new ValueNumbering(config)
    val typecheckerBefore = new Typechecker {}
    typecheckerBefore.checkProgram(Seq(input))
    println(s"before VN: \n$input")
    val result = VN.valueNumbering(input)
    val typecheckerAfter = new Typechecker {}
    typecheckerAfter.checkProgram(Seq(result))
    println(s"after VN: \n$result")
    assertResult(expected)(result)
  }


}



