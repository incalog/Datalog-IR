package inca.ir.extension.demand

import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Eq, Module, ModuleEntry, Param, Relation, TAny, Var, string2name, termList2ArgList}
import inca.ir.extension.demand
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.optimize.AliasElimination
import org.scalatest.funsuite.AnyFunSuiteLike

class DemandLoweringWithSupplementariesTest extends AnyFunSuiteLike:

  def module(relations: ModuleEntry*): Module =
    val typecheckerBefore = new IRTypechecker
    val typecheckerLowered = new IRTypechecker
    val typecheckerOptimized = new IRTypechecker
    val lowering = new demand.LoweringWithSupplementaries {}

    val mod = Module("M", BaseIR.language + demand.IR + arithmetic.IR, relations)
    var printedMod = false
    var lowered: Module = null
    var optimized: Module = null
    try {
      typecheckerBefore.checkProgram(Seq(mod))
      println(mod)
      printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerLowered.checkProgram(Seq(lowered))

      val optimization = new AliasElimination {}
      optimized = optimization.visitProgram(Seq(lowered)).head
      typecheckerOptimized.checkProgram(Seq(lowered))

      lowered
    } finally {
      if (!printedMod)
        println(mod)
      println()
      println("Lowered:")
      println(lowered)

      println()
      println("Optimized:")
      println(optimized)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsLowered = typecheckerLowered.getErrors
      val errorsOptimized = typecheckerOptimized.getErrors
      if (errorsBefore.nonEmpty) {
        println("Type errors in original code:")
        errorsBefore.foreach(println)
      }
      if (errorsLowered.nonEmpty) {
        println("Type errors in lowered code:")
        errorsLowered.foreach(println)
      }
      if (errorsOptimized.nonEmpty) {
        println("Type errors in optimized code:")
        errorsOptimized.foreach(println)
      }
    }

  test("demand propagates") {
    val m1 = module(
      Relation("Q", Seq(Param("x", TDemand(TAny)), Param("y", TDemand(TAny))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("x"))),
        Call("R", Seq(Var("y"), Var("y"))),
        Eq(Var("x"), Var("y")),
        Call("R", Seq(Var("y"), Var("x")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TAny)), Param("p2", TDemand(TAny))), Seq(Body(Seq(
      ))))
    )
  }

  /*
   * Q(x: TInt, y: TInt) :- R(x, x), a = 1, R(y, a).
   * R(p1: TInt, p2: TInt).
   * ~>
   * Q(x: TInt, y: TInt) :- Q$input(x, y), R(x, x), a = 1, R(y, a).
   * R(p1: TInt, p2: TInt) :- R$input(p1, p2).
   *
   * Q$input(x$0: TInt, y$0: TInt) :- nil.
   * // varsBefore = [x, y]
   * Q_R$0(x: TInt, y: TInt) :- Q$input(x, y).
   * // varsBefore = [x, y, a]
   * Q_R$1(x: TInt, y: TInt, a: TInt) :- Q_R$0(x, y), a = 1.
   *
   * R$input(p1$0: TInt, p2$0: TInt) :- Q_R$0(x, y), p1$0 == x, p2$0 == x.
   * R$input(p1$0: TInt, p2$0: TInt) :- Q_R$1(x, y, a), p1$0 == y, p2$0 == a.
   */

  test("demand propagates with Int") {
    val m1 = module(
      Relation("Q", Seq(Param("x", TDemand(TInt)), Param("y", TDemand(TInt))), Seq(Body(Seq(
        Call("R", Seq(Var("x"), Var("x"))),
        Eq(Var("a"), IntNum(1)),
        Call("S", Seq(Var("a"))),
        Call("R", Seq(Var("y"), Var("a")))
      )))),
      Relation("R", Seq(Param("p1", TDemand(TInt)), Param("p2", TDemand(TInt))), Seq(Body(Seq()))),
      Relation("S", Seq(Param("p1", TDemand(TInt))), Seq(Body(Seq())))
    )
  }
