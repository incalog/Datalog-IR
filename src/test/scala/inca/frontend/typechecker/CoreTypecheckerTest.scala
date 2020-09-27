package inca.frontend.typechecker

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.parser.{CoreParser, _}
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.core.Core._
import inca.frontend.util.Program

/**
  * Test class for the IncA core language typechecker.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreTypecheckerTest extends AnyFunSuite {
  test("test subtype") {
    def test_run(t1 : TypeAnno, t2 : TypeAnno) = new CoreTypechecker(null, Program(Seq.empty), Seq()).subtype(t1, t2)

    test_run(TBool, TBool)
    test_run(TBool, TAny)
    test_run(TString, TAny)
    test_run(TNode("apf3l"), TAnyLinked)
  }

  test("test Typechecker") {
    def test_run(cd: String) = {
      parse(cd, CoreParser().module(_)) match {
        case Success(value, index) => {
          new CoreTypechecker(null, Program(Seq(value)), Seq()).typecheck() match {
            case SuccessTypecheck(warnings)     =>
            case FailTypecheck(error, warnings) => fail(s"$error, $warnings")
          }
        }
        case Failure(label, index, extra) =>
          fail(s" ${cd.slice(index - 3, index + 3)} $label, $index, $extra")
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
          |def another() : Int = {
          |    yield name()
          |} """.stripMargin,
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
          |} """.stripMargin
    )
    code.map(test_run)
  }

  test("test imports") {
    val mod1Src =
      """
        |module test1
        |
        |def hello(): String = {
        |  yield "Hello World"
        |}
        |""".stripMargin
    val mod1 = parse(mod1Src, CoreParser().module(_)).get.value
    val src =
      """
        |module main
        |import test1
        |
        |def main(): Unit = {
        |  val x = hello()
        |}
        |""".stripMargin

    val code = parse(src, CoreParser().module(_)).get.value
    val prog = Program(Seq(mod1, code))
    val typechecker = new CoreTypechecker(null, prog, Seq.empty)
    typechecker.typecheck() match {
      case SuccessTypecheck(warnings) =>
        println(warnings)
      case FailTypecheck(errors, warnings) =>
        println(errors)
        fail()
    }
  }

}
