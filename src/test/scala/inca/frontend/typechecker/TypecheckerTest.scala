package inca.frontend.typechecker

import inca.frontend.parser.Parser
import fastparse.Parsed.{Failure, Success}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.core.Core._
import inca.frontend.util.Program

/**
  * Test class for the IncA core language typechecker.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class TypecheckerTest extends AnyFunSuite {
  test("test Typechecker") {
    def test_run(cd: String) = {
      Parser.parseModule(cd) match {
        case Success(value, index) => {
          val prog = Program(Seq(value))
          Typechecker.typecheck(null, prog) match {
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
          |    val x = 4
          |    yield eval(x + 38)
          |} """.stripMargin,
      s"""|
          |module test
          |
          |def name() : Any = {
          |    val x = 5
          |    yield x
          |} """.stripMargin,
      s"""|
          |module test
          |
          |def name() : Any = {
          |    val x = true 
          |    assert x instanceOf Any
          |    yield x
          |} """.stripMargin,
      s"""|
          |module test
          |
          |def name() : Int = {
          |  val x = 9
          |  if (1 == 2) {
          |    yield 6
          |  } else    {
          |    yield 2
          |  }
          |}
          |
          |def name2() : Int = {
          |  val x = 7
          |  if (x == 2) {
          |    x match {
          |      case 2 => {
          |        yield 2
          |      }
          |      case z => {
          |        yield z
          |      }
          |      case _ => {
          |        yield 4
          |      }
          |    }
          |  } else {
          |    yield 2
          |  }
          |}""".stripMargin
    )

    code.map(test_run)
  }

}
