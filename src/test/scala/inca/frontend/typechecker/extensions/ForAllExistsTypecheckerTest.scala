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
        Name("x"),
        evalIterable,
        Body(
          Assign(Seq(Name("y")), Var("x")),
          Yield(Constant(BooleanLiteral(true)))
        )
      )
    )

    val typechecker = getTypechecker(code, TUnit, Seq(ForAllExistsTypechecker))
    checkTypecheck(typechecker)
  }

  test("test exists") {
    val code = Body(
      Exists(
        Name("x"),
        evalIterable,
        Body(
          Assign(Seq(Name("y")), Var("x")),
          Yield(Constant(BooleanLiteral(true)))
        )
      )
    )

    val typechecker = getTypechecker(code, TUnit, Seq(ForAllExistsTypechecker))
    checkTypecheck(typechecker)
  }

  def checkTypecheck(typechecker: CoreTypechecker): Unit = {
    val result = typechecker.typecheck()
    result match {
      case FailTypecheck(errors, warnings) =>
        fail(errors.mkString("\n"))
      case SuccessTypecheck(warnings) =>
        warnings.mkString("\n")
    }
  }
}
