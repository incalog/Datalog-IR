package inca.ir.typing

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.{arithmetic, demand, not}
import inca.ir.extension.not.Not
import inca.ir.typing.deps.{FunctionalDependency, FunctionalDependencyHint}
import inca.ir.{term2Arg, *}
import inca.ir.typing.{BaseIRTypechecker, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

class FunctionalDependencyTest extends AnyFunSuiteLike:

  def typechecker() = new BaseIRTypechecker with deps.Typechecker with arithmetic.Typechecker {}

  def module(relations: Relation*): Module =
    val mod = Module("M", BaseIR.language, relations)
    val checker = typechecker()
    try checker.checkProgram(Seq(mod))
    finally {
      println(mod)
      checker.getErrors.foreach(println)
    }
    mod


  test("call binds arguments") {
    val paramPred = Param("k", TInt)
    val paramSucc = Param("v", TInt)
    module(
      Relation("Inc", Seq(paramPred, paramSucc), Seq(Body(Seq(
        Eq(Var(paramPred.name), IntNum(1)), Eq(Var(paramSucc.name), IntNum(2))
      ))))
        .addHint(FunctionalDependencyHint(
          FunctionalDependency(Seq(paramPred), Seq(paramSucc)),
          FunctionalDependency(Seq(paramSucc), Seq(paramPred))
        )),

      Relation("T", Seq(Param("x1", TInt), Param("x2", TInt)), Seq(Body(Seq(
        Call("Inc", Seq(Var("x1"), Var("x2")))
      ))))
    )
  }
