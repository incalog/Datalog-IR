package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions.{Exists, Forall}
import inca.frontend.typechecker.TestingUtils._
import inca.frontend.typechecker.{CoreTypechecker, FailTypecheck, SuccessTypecheck}
import org.scalatest.funsuite.AnyFunSuite

class ForAllExistsTypecheckerTest extends AnyFunSuite{

  test("test forall") {
    val code = Body(
      Forall(
        "x",
        evalIterable,
        Body(
          Assign(Seq("y"), Var("x")),
          Yield(Constant(BooleanLiteral(true)))
        )
      )
    )

    val typechecker = getTypechecker(code, TBool, Seq(ForAllExistsTypechecker))
    checkTypecheck(typechecker)
  }

  test("test exists") {
    val code = Body(
      Exists(
        "x",
        evalIterable,
        Body(
          Assign(Seq("y"), Var("x")),
          Yield(Constant(BooleanLiteral(true)))
        )
      )
    )

    val typechecker = getTypechecker(code, TBool, Seq(ForAllExistsTypechecker))
    checkTypecheck(typechecker)
  }

  def checkTypecheck(typechecker: CoreTypechecker): Unit = {
    val result = typechecker.typecheck()
    result match {
      case FailTypecheck(errors, warnings) =>
        fail(errors.toString() + typechecker.prog.prettyprint)
      case SuccessTypecheck(warnings) =>
    }
  }
}
