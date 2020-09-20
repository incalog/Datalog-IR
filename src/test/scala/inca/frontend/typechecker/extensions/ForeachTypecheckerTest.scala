package inca.frontend.typechecker.extensions

import inca.frontend.core.Core._
import inca.frontend.extensions.Foreach
import inca.frontend.typechecker.TestingUtils._
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class ForeachTypecheckerTest extends AnyFunSuite {

  test("test foreach") {

    val code = Body(
      Foreach(
        "x",
        evalIterable,
        Body(
          Assign(Seq("y"), Eval(Seq.empty, q"inca.analyzedData.Nat.Zero"))
        )
      ),
      Yield(Var("x"))
    )

    val typechecker = getTypechecker(code, TInt, Seq(ForeachTypechecker))
    checkTypecheck(typechecker, this)
  }
}
