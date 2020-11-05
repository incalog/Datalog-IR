package inca.frontend.parser

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.core._
import org.scalatest.Assertion
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.{Import => _, Name => _, _}

/**
  * Test class for the IncA core language parser @see Parser.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class CoreNativeParserTest extends AnyFunSuite {

  val parser = new CoreParser
  
  test("test Module") {
    def testModule = testSuccess[Module](parser.module(_))

    testModule(
      s"""module my
                |import math
                |import cuda_runtime
                |scala import java.lang
                |scala import inca.Compiler
                |""".stripMargin,
      Module(
        Name("my"),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(ScalaModuleContent(q"import java.lang"), ScalaModuleContent(q"import inca.Compiler"))
      )
    )

    testModule(
      s"""module my
         |
         |import math
         |import cuda_runtime
         |
         |scala import inca.Compiler
         |
         |scala val i = 0
         |scala var v: Int = 0
         |scala def f() = { Compiler.invoke() }
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(
          q"import inca.Compiler",
          q"val i = 0",
          q"var v: Int = 0",
          q"def f() = { Compiler.invoke() }").map(ScalaModuleContent.apply)
      )
    )

  }


  private def testSuccess[T](parser: P[_] => P[Any]): (String, T) => Assertion =
    (input: String, cmp: T) => {
      parse(input, parser) match {
        case Success(value, index)        =>
          assert(value === cmp)
          assertResult(input.length)(index)
        case Failure(label, index, extra) => fail(s"$label, $index, $extra")
      }
    }

  private def testFailure[T](parser: P[_] => P[Any]): String => Unit =
    (input: String) => {
      parse(input, parser) match {
        case Success(value, index) if input.length == index => fail(s"Expected failed parsing, but got $value")
        case Success(value, index) if input.length != index =>
        case Failure(label, index, extra) =>
      }
    }
}
