package inca.ir.optimization

import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.map.*
import inca.ir.extension.*
import inca.ir.extension.data.TData
import inca.ir.extension.edbdata.*
import inca.ir.lowering.BaseLowering
import inca.ir.typing.IRTypechecker
import inca.ir.{Body, Call, Cast, Eq, Language, Module, ModuleEntry, Param, Relation, TNothing, Var, string2name}
import org.scalatest.funsuite.AnyFunSuiteLike


class DemandedMapLookupTest extends AnyFunSuiteLike {

  def module(optimize: Boolean, relations: Seq[ModuleEntry]): Module =
    val typecheckerBefore = new IRTypechecker

    val mapLowering = Seq(
      new map.Lowering {},
      new disjunction.Lowering {},
      new tuple.Lowering {},
      new block.Lowering {},
      new demand.Lowering {}
    )

    def lower(l: BaseLowering, m: Module): Module =
      val checker = new IRTypechecker
      try checker.checkProgram(Seq(m))
      //finally checker.getErrors.foreach(println)
      //println(s"Lowering ${l.name}")
      val lowered = l.lower(m)
      lowered

    val mod = Module("M", Language(map.IR, arithmetic.IR, disjunction.IR, tuple.IR, block.IR, demand.IR), relations)
    try typecheckerBefore.checkProgram(Seq(mod))
    finally println(mod)
    val lowered = mapLowering.foldLeft(mod)((mod, l) => lower(l, mod))
    val checker = new IRTypechecker
    try checker.checkProgram(Seq(lowered))
    println(lowered)
    lowered


  def e = Var("e")
  def e(n: Int) = Var(s"e_$n")
  def x = Var("x")
  def xStr = Var("xStr")
  def ctx = Var("ctx")

  def checkRelation = Relation(
    "check",
    Seq(
      Param(ctx.name, TDemand(TMap(TString, TInt))),
      Param(e.name, TDemand(TEdbNode("Exp")))
    ),
    Seq(
      Body(
        EdbDeconstruct(e, "Var", "name" -> x) :+
          Eq(Var("dummy"), MapLookUp(ctx, Cast(x, TString)))
      ),
      Body(
        EdbDeconstruct(e, "Num")
      ),
      Body(
        EdbDeconstruct(e, "Add", "lhs" -> e(1), "rhs" -> e(2)) :+
          Call("check", Seq(ctx.arg, e(1).arg)) :+
          Call("check", Seq(ctx.arg, e(2).arg))
      ),
      Body(
        EdbDeconstruct(e, "Let", "name" -> x, "bound" -> e(1), "body" -> e(2)) :+
          Call("check", Seq(ctx.arg, e(1).arg)) :+
          Eq(xStr, Cast(x, TString)) :+
          Call("check", Seq(MapPlus(ctx, xStr, IntNum(1)).arg, e(2).arg))
      )
    )
  )
  def mainRelation = Relation(
    "main",
    Seq(Param(e.name, TEdbNode("Exp"))),
    Seq(Body(Seq(
      UndefEdbField(e, Link.Parent),
      Call("check", Seq(MapLit(Seq()).arg, e.arg))
    )))
  )

  test("Exp name check, no optimization"){
    module(false, edbdata.edbExp :+ checkRelation :+ mainRelation)
  }

//  test("Exp name check, manual lookup") {
//    def checkRelation = Relation(
//      "check",
//      Seq(
//        Param(ctx.name, TMap(TString, TInt)),
//        Param(e.name, TEdbNode("Exp"))
//      ),
//      Seq(
//        Body(
//          EdbDeconstruct(e, "Var", "name" -> x) :+
//            Eq(Var("dummy"), MapLookUp(ctx, Cast(x, TString)))
//        ),
//        Body(
//          EdbDeconstruct(e, "Num")
//        ),
//        Body(
//          EdbDeconstruct(e, "Add", "lhs" -> e(1), "rhs" -> e(2)) :+
//            Call("check", Seq(ctx.arg, e(1).arg)) :+
//            Call("check", Seq(ctx.arg, e(2).arg))
//        ),
//        Body(
//          EdbDeconstruct(e, "Let", "name" -> x, "bound" -> e(1), "body" -> e(2)) :+
//            Call("check", Seq(ctx.arg, e(1).arg)) :+
//            Eq(xStr, Cast(x, TString)) :+
//            Call("check", Seq(MapPlus(ctx, xStr, IntNum(1)).arg, e(2).arg))
//        )
//      )
//    )
//    def mainRelation = Relation(
//      "main",
//      Seq(Param(e.name, TEdbNode("Exp"))),
//      Seq(Body(Seq(
//        Call("check", Seq(MapLit(Seq()).arg, e.arg))
//      )))
//    )
//    def checkInput = Relation(
//      "check$input",
//      Seq(
//        Param("ctx", TMap(TString, TInt)),
//        Param("e", TEdbNode("Exp"))
//      ),
//      Seq(
//
//      )
//    )
//    def lookupRelation = Relation(
//      "lookup",
//      Seq(
//        ???
//      ),
//      ???
//    )
//    module(true, edbdata.edbExp :+ checkRelation :+ mainRelation)
//  }

}
