package inca.frontend.typechecker.extensions

import inca.frontend.core.Core.{AnnoParam, Assert, Body, BooleanLiteral, Constant, DoubleLiteral, IntLiteral, Module, Param, PatternFunction, TAny, TInt, TypeAnno, UnitLiteral, Var, Yield}
import inca.frontend.extensions.{ElseIf, IfThenElse}
import inca.frontend.typechecker.{CoreTypechecker, FailTypecheck, SuccessTypecheck}
import inca.frontend.util.Program
import org.apache.log4j.ConsoleAppender
import org.scalatest.funsuite.AnyFunSuite

class IfThenElseTypecheckerTest extends AnyFunSuite{

  def buildFun(body: Body, expected: TypeAnno) = PatternFunction(None, "default", Seq(Param("x", TInt)), Seq(AnnoParam(None, expected)), Seq(body))

  def buildMod(body: Body, expected: TypeAnno) = Module("default", Seq.empty, Seq(buildFun(body, expected)))

  test("test typecheck only If statement") {
    val code = Body(
      IfThenElse(
        Constant(BooleanLiteral(true)),
        Body(
          Assert(Constant(BooleanLiteral(false)))
        ),
        Seq.empty,
        None
      ),
      Yield(Var("x"))
    )
    val typechecker = new CoreTypechecker(null, Program(Seq(buildMod(code, TInt))), Seq(IfThenElseTypechecker))
    checkTypecheck(typechecker)
  }

  test("test typecheck if-else") {
    val code = Body(
      IfThenElse(
        Constant(BooleanLiteral(true)),
        Body(
          Yield(Var("x"))
        ),
        Seq.empty,
        Some(
          Body(
            Yield(Constant(DoubleLiteral(1.2)))
          )
        )
      )
    )
    val typechecker = new CoreTypechecker(null, Program(Seq(buildMod(code, TAny))), Seq(IfThenElseTypechecker))
    checkTypecheck(typechecker)
  }

  test("test typecheck if-elseif-else") {
    val code = Body(
      IfThenElse(
        Constant(BooleanLiteral(true)),
        Body(
          Yield(Var("x"))
        ),
        Seq(
          ElseIf(
            Constant(BooleanLiteral(true)),
            Body(
              Yield(Constant(IntLiteral(0)))
            )
          ),
          ElseIf(
            Constant(BooleanLiteral(false)),
            Body(
              Yield(Var("x"))
            )
          )
        ),
        Some(
          Body(
            Yield(Constant(BooleanLiteral(false)))
          )
        )
      )
    )
    val typechecker = new CoreTypechecker(null, Program(Seq(buildMod(code, TAny))), Seq(IfThenElseTypechecker))
    checkTypecheck(typechecker)
  }

  test("test nested IfThenElse") {
    val code = Body(
      IfThenElse(
        Constant(BooleanLiteral(true)),
        Body(
          IfThenElse(
            Constant(BooleanLiteral(false)),
            Body(
              Assert(Constant(BooleanLiteral(true)))
            ),
            Seq.empty,
            None
          )
        ),
        Seq.empty,
        None
      ),
      Yield(Var("x"))
    )
    val typechecker = new CoreTypechecker(null, Program(Seq(buildMod(code, TInt))), Seq(IfThenElseTypechecker))
    checkTypecheck(typechecker)
  }

  def checkTypecheck(typechecker: CoreTypechecker): Unit = {
    val result = typechecker.typecheck()
    result match {
      case FailTypecheck(errors, warnings) =>
        println(errors)
        fail()
      case SuccessTypecheck(warnings) =>
    }
  }
}
