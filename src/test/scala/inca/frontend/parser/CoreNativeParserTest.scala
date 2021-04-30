package inca.frontend.parser

import fastparse.Parsed.{Failure, Success}
import fastparse._
import inca.frontend.core
import inca.runtime.context.DataModel
import inca.util.Meta.Scala
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

  val parser = new CoreParser {}
  import inca.frontend.core.tree._
  
  test("test Module") {
    def testModule = testSuccess[Module](parser.module(_))

    testModule(
      s"""module my
                |import math
                |import cuda_runtime
                |`import java.lang`
                |`import inca.Compiler`
                |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(),
        Seq(ScalaModuleContent(Scala(q"import java.lang")), ScalaModuleContent(Scala(q"import inca.Compiler")))
      )
    )

    testModule(
      s"""module my
         |
         |import math
         |import cuda_runtime
         |```
         |import java.lang
         |import inca.Compiler
         |```
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(),
        Seq(ScalaModuleContent(Scala(q"import java.lang")), ScalaModuleContent(Scala(q"import inca.Compiler")))
      )
    )

    testModule(
      s"""module my
         |
         |import math
         |import cuda_runtime
         |
         |`import inca.Compiler`
         |
         |`val i = 0`
         |`var v: Int = 0`
         |`def f() = { Compiler.invoke() }`
         |""".stripMargin,
      Module(
        Name("my"),
        Seq(),
        Seq(Import(Name("math")), Import(Name("cuda_runtime"))),
        Seq(),
        Seq(
          q"import inca.Compiler",
          q"val i = 0",
          q"var v: Int = 0",
          q"def f() = { Compiler.invoke() }").map(s => ScalaModuleContent(Scala(s)))
      )
    )

    testModule(
      s"""module Test
         |
         |```
         |trait Nat
         |case object Zero extends Nat
         |case class Succ(pred: Nat) extends Nat
         |```
         |def testTwo(): `Nat` = {
         |  yield `Succ(Zero)`
         |}
         |""".stripMargin,
      Module(
        Name("Test"),
        Seq(),
        Seq(),
        Seq(),
        Seq(
          q"trait Nat",
          q"case object Zero extends Nat",
          q"case class Succ(pred: Nat) extends Nat"
        ).map(s => ScalaModuleContent(Scala(s)))
        :+ PatternFunction(None, Name("testTwo"), Seq(), TScala("Nat"),
             Seq(Body(Seq(Yield(Eval(Scala(q"Succ(Zero)")))))))
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
