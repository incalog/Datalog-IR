package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic
import inca.ir.*
import inca.ir.valueNumbering.{ArithmeticValueNumbering, BaseValueNumbering, ConfigVN, ValueNumbering}
import inca.ir.typing.Typechecker

abstract class ValueNumberingTestAbstract extends AnyFunSuite{

  val config: ConfigVN = ConfigVN()
  
  
  /** for testing VN without any additional knowledge about extensions */
  def performTestWithBaseVN(expected: IRModule, input: IRModule, config: ConfigVN = config): Unit = {
    val VN = new BaseValueNumbering() {}
    performTestInternal(expected, input, VN)
  }
  
  def performTest(expected: IRModule, input: IRModule, config: ConfigVN = config): Unit = {
    val VN = new ValueNumbering(config)
    performTestInternal(expected, input, VN)
  }
  
  private def performTestInternal(expected: IRModule, input: IRModule, VN: BaseValueNumbering): Unit = {
    val typecheckerBefore = new Typechecker {}
    typecheckerBefore.checkProgram(Seq(input))
    println(s"before VN: \n$input\n")
    val result = VN.valueNumbering(input)
    val typecheckerAfter = new Typechecker {}
    println(s"after VN: \n$result")
    typecheckerAfter.checkProgram(Seq(result))
    assertResult(expected)(result)
    println("#" * 100)
  }

}



