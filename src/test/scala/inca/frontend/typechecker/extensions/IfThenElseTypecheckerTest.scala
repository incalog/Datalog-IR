package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core.{AnnoParam, Assert, Body, BooleanLiteral, Constant, DoubleLiteral, IntLiteral, Module, Param, PatternFunction, TAny, TInt, TypeAnno, UnitLiteral, Var, Yield}
import inca.frontend.extensions.{ElseIf, IfThenElse}
import inca.frontend.typechecker.{CoreTypechecker, FailTypecheck, SuccessTypecheck}
import inca.frontend.util.Program
import org.scalatest.funsuite.AnyFunSuite
import inca.frontend.typechecker.TestingUtils._

class IfThenElseTypecheckerTest extends AnyFunSuite{

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
    checkTypecheck(typechecker, this)
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
    checkTypecheck(typechecker, this)
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
    checkTypecheck(typechecker, this)
  }

  test("test nested IfThenElse") {
    val code = Body(
      IfThenElse(
        Constant(BooleanLiteral(true)),
        Body(
          IfThenElse(
            Constant(BooleanLiteral(false)),
            Body(
              Assert(Constant(BooleanLiteral(true))),
              Yield(Var("x"))
            ),
            Seq.empty,
            Some(
              Body(
                Yield(Constant(IntLiteral(1)))
              )
            )
          )
        ),
        Seq.empty,
        Some(
          Body(
            Yield(Var("x"))
          )
        )
      )
    )
    val typechecker = new CoreTypechecker(null, Program(Seq(buildMod(code, TInt))), Seq(IfThenElseTypechecker))
    checkTypecheck(typechecker, this)
  }

}
