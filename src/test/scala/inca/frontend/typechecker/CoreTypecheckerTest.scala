package inca.frontend.typechecker

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.parser.{CoreParser, _}
import org.scalatest.funsuite.AnyFunSuite

/**
  * Test class for the IncA core language typechecker.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreTypecheckerTest extends AnyFunSuite {
  test("test Typechecker") {
    def test_run(cd: String) = {
      parse(cd, CoreParser().module(_)) match {
        case Success(value, index) => {
          new CoreTypechecker(null, Program(Seq(value)), Seq()).typecheck() match {
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
          |}""".stripMargin,
      s"""|
          |module test
          |
          |def name() : Boolean = {
          |    val x = 5
          |    assert x instanceOf Boolean
          |    yield x
          |} """.stripMargin
    )

    code.map(test_run)
  }
}
