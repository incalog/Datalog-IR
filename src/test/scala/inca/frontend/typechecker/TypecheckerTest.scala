package inca.frontend.typechecker

import fastparse.Parsed.{Failure, Success}
import inca.analyzedLangs.Exp
import inca.frontend.Frontend
import inca.frontend.parser.Conversions._
import inca.frontend.util.Program
import org.scalatest.funsuite.AnyFunSuite

/**
  * Test class for the IncA core language typechecker.
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
class TypecheckerTest extends AnyFunSuite {

  val lmi = Exp.languageMetaInfo

  test("test Typechecker") {
    // println(lmi.links)
    def test_run(cd: String) = {
      Frontend.Inca.parseModule(cd) match {
        case Success(value, index) => {
          val prog = Program(Seq(value))
          Typechecker.typecheck(lmi, prog) match {
            case SuccessTypecheck(warnings)     =>
            case FailTypecheck(error, warnings) => fail(s"$error, $warnings")
          }
        }
        case Failure(label, index, extra) =>
          fail(s" CODE AROUND FAILURE: ${cd.slice(index - 5, index + 5)}")
      }
    }

    val code = Seq(
      s"""module test
          |
          |def name() : Int = {
          |    val x = 5
          |    yield x 
          |}""".stripMargin,
      s"""module test
          |
          |def name() : Unit = {
          |    val x = 5
          |}""".stripMargin,
      s"""module test
          |
          |def name() : Int = {
          |    val x = 5
          |    yield x
          |} union {
          |    yield 10
          |}""".stripMargin,
      s"""module test
          |
          |def name() : Int = {
          |    val x = 4
          |    yield eval(x + 38)
          |} """.stripMargin,
      s"""module test
          |
          |def name() : Any = {
          |    val x = 5
          |    yield x
          |} """.stripMargin,
      s"""module test
          |
          |def name() : Any = {
          |    val x = true 
          |    assert x instanceOf Any
          |    yield x
          |} """.stripMargin,
      s"""module test
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
          |}""".stripMargin,
      s"""module test
         |
         |def isEven(x: Int): Boolean = {
         |  yield eval(x % 2 == 0)
         |}
         |
         |def check(t: Any): Int = {
         |  t match {
         |    case inca.analyzedLangs.Exp.Add(lhs=lhs, rhs=rhs) => {
         |      val l = check(lhs)
         |      val r = check(rhs)
         |      yield eval(l + r)
         |    }
         |    case exp@inca.analyzedLangs.Exp() => {
         |      yield 5
         |    }
         |    case somethingElse => {
         |      yield 1
         |    }
         |  }
         |} """.stripMargin
    )
    code.map(test_run)
  }

  test("test typchecker complex") {
    val lmi = Exp.languageMetaInfo
    val code =
      mod"""module test
        def countNodes(exp: inca.analyzedLangs.Exp): Int = {
          exp match {
            case inca.analyzedLangs.Exp.BooleanLit() => {
              yield 1
            }
            case inca.analyzedLangs.Exp.IntegerLit() => {
              yield 1
            }
            case inca.analyzedLangs.Exp.LongLit() => {
              yield 1
            }
            case inca.analyzedLangs.Exp.Mul(lhs=lhs, rhs=rhs) => {
               val l = countNodes(lhs)
               val r = countNodes(rhs)
               yield eval(l + r)
             }
            case inca.analyzedLangs.Exp.Add(lhs=lhs, rhs=rhs) => {
              val l = countNodes(lhs)
              val r = countNodes(rhs)
              yield eval(l + r)
            }
            case inca.analyzedLangs.Exp.Not(e=e) => {
              yield countNodes(e)
            }
            case inca.analyzedLangs.Exp.And(lhs=lhs, rhs=rhs) => {
              val l = countNodes(lhs)
               val r = countNodes(rhs)
               yield eval(l + r)
             }
             case inca.analyzedLangs.Exp.Or(lhs=lhs, rhs=rhs) => {
               val l = countNodes(lhs)
               val r = countNodes(rhs)
               yield eval(l + r)
             }
             case many@inca.analyzedLangs.Exp.Many(exps=exps) => {
               // no idea how to implement it
               yield many.children.size
             }
           }
         }
         """

    // println(code)
    Typechecker.typecheck(lmi, Program(Seq(code))) match {
      case SuccessTypecheck(warnings) =>
        println(warnings)
      case FailTypecheck(errors, warnings) =>
        // println(errors)
        fail(errors.mkString("\n"))
    }
  }

  test("test leastCommonType") {
    val checker = new CoreTypechecker(lmi, Program(Seq.empty), Seq.empty)
    assert(checker.leastCommonType(truechange.SortType("inca.analyzedLangs.Exp.BooleanLit"), truechange.SortType("inca.analyzedLangs.Exp.BooleanLit")) === Some(truechange.SortType("inca.analyzedLangs.Exp.BooleanLit")))
    assert(checker.leastCommonType(truechange.SortType("inca.analyzedLangs.Exp.And"), truechange.SortType("inca.analyzedLangs.Exp.BooleanLit")) === Some(truechange.SortType("inca.analyzedLangs.Exp")))
    assert(checker.leastCommonType(truechange.SortType("inca.analyzedLangs.Exp.Or"), truechange.SortType("inca.analyzedLangs.Exp.And")) === Some(truechange.SortType("inca.analyzedLangs.Exp")))
  }
}
