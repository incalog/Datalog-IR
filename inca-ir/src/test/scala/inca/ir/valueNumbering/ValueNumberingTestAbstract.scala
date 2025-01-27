package inca.ir.valueNumbering

import inca.ir.Module as IRModule
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.*
import inca.ir.typing.Typechecker
import inca.ir.valueNumbering.BaseVN.BaseValueNumbering

abstract class ValueNumberingTestAbstract extends AnyFunSuite{

  /** wraps parameters for value numbering */
  case class ConfigVN(normalizeDoubles: Boolean = false,
                      useDefiningTerm: Boolean = false,
                      useFixPointIteration: Boolean = true,
                      useGlobalPropagationOfConstLeaders: Boolean = true,
                      printVNResults: Boolean = true,
                      printBeforeAfter: Boolean = true,
                      printVNStatistics: Boolean = true
                     )
  
  val config: ConfigVN = ConfigVN()
  
  
  def performTest(expected: IRModule, input: IRModule, config: ConfigVN = config): Unit = {
    val VN: ValueNumbering = new ValueNumbering {
      override val normalizeDoubles: Boolean = config.normalizeDoubles
      override val useDefiningTerm: Boolean = config.useDefiningTerm
      override val useFixPointIteration: Boolean = config.useFixPointIteration
      override val useGlobalPropagationOfConstLeaders: Boolean = config.useGlobalPropagationOfConstLeaders
      override val printVNResults: Boolean = config.printVNResults
      override val printBeforeAfter: Boolean = config.printBeforeAfter
      override val printVNStatistics: Boolean = config.printVNStatistics
    }
    performTestInternal(expected, input, VN)
  }
  
  private def performTestInternal(expected: IRModule, input: IRModule, VN: BaseValueNumbering): Unit = {
    val typecheckerBefore = new Typechecker {}
    typecheckerBefore.checkProgram(Seq(input))
    val result = VN.valueNumbering(input)
    val typecheckerAfter = new Typechecker {}
    typecheckerAfter.checkProgram(Seq(result))
    assertResult(expected)(result)
    println("#" * 100)
  }

}



