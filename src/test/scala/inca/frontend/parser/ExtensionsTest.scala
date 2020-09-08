package inca.frontend.parser

import inca.frontend.core.Core
import inca.frontend.core.Core._
import org.scalatest.funsuite.AnyFunSuite
import fastparse._
import fastparse.Parsed.Success
import fastparse.Parsed.Failure
import inca.frontend.parser.extensions.BoolOpsParser
import inca.frontend.extensions._

class ExtensionsTest extends AnyFunSuite {

  test("test BoolOps") {
    def test_run = test_helper(CoreParser(Seq(BoolOpsParser)).exp(_))

    test_run("x && y", And(Var("x"), Var("y")))
    test_run("x || y", Or(Var("x"), Var("y")))
    test_run("!x", Not(Var("x")))
    test_run("!(x && (y || z))", Not(And(Var("x"), Or(Var("y"), Var("z")))))
  }

  private def test_helper[T](parser: P[_] => P[Any]) =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        => assert(cmp === value)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private[parser] def test_helper_negative[T](parser: P[_] => P[Any]) =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index)        => fail(s"$value, $index")
        case Failure(label, index, extra) =>
      }
    }
}
