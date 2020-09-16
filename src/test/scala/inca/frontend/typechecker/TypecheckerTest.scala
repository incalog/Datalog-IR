package inca.frontend.typechecker

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.typechecker.Typechecker
import inca.frontend.parser.CoreParser
import inca.frontend.parser._

/**
  * Test class for the IncA core language typechecker.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class TypecheckerTest extends AnyFunSuite {
  test("test Typechecker") {
    def test_run(cd: String) = {
      parse(cd, CoreParser().module(_)) match {
        case Success(value, index) => {
          new Typechecker(null, Program(Seq(value))).typecheck() match {
            case SuccessTypecheck(warnings)     =>
            case FailTypecheck(error, warnings) => fail(s"$error, $warnings")
          }
        }
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

    val code = Seq(
      s"""|module test
          |
          |def name() : Int = {
          |    val x = 5
          |    yield x 
          |}""".stripMargin,
      s"""|module test
          |
          |def name() : Unit = {
          |    val x = 5
          |}""".stripMargin,
      s"""|
          |module test
          |
          |def name() : Int = {
          |    val x = 5
          |    yield x
          |} union {
          |    yield 10
          |}""".stripMargin,
      s"""|
          |module test
          |
          |def name() : Int = {
          |    val x = 5
          |    yield x
          |} 
          |
          |def another() : Int {
          |    yield name()
          |}""".stripMargin
    )

    code.map(test_run)
  }
}
