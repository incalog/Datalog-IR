package inca.ir.extension.map

import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.{block, demand, disjunction, tuple, arithmetic}
import inca.ir.lowering.BaseLowering
import inca.ir.typing.IRTypechecker
import inca.ir.{Body, Eq, Language, Module, Param, Relation, TNothing, Var, string2name}
import org.scalatest.funsuite.AnyFunSuiteLike


class MapTest extends AnyFunSuiteLike {
  def module(relations: Relation*): Module =
    val typecheckerBefore = new IRTypechecker

    val mapLowering = Seq(
      new Lowering {},
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
      //println(lowered)
      lowered

    val mod = Module("M", Language(IR, arithmetic.IR, disjunction.IR, tuple.IR, block.IR, demand.IR), relations)
    typecheckerBefore.checkProgram(Seq(mod))
    //println(mod)
    val lowered = mapLowering.foldLeft(mod)((mod, l) => lower(l, mod))
    mod

  test("Empty map"){
    val mainRelation = Relation(
      "main",
      Seq(
        Param("m", TMap(TNothing, TNothing))
      ),
      Seq(Body(Seq(
        Eq(Var("m"), MapLit.empty)
      )))
    )
    module(mainRelation)
  }
  
  test("Map Literal"){
    val mainRelation = Relation(
      "main",
      Seq(Param("m", TMap(TString, TInt))),
      Seq(Body(Seq(
        Eq(Var("m"), MapLit.from((StringLit("A"), IntNum(1)), (StringLit("B"), IntNum(2))))
      )))
    )
    module(mainRelation)
  }
  
  test("Map union"){
    val mainRelation = Relation(
      "main",
      Seq(Param("m2", TMap(TString, TInt))),
      Seq(Body(Seq(
        Eq(Var("m1"), MapLit.from((StringLit("A"), IntNum(1)))),
        Eq(Var("m2"), MapUnion(Var("m1"), MapLit.from((StringLit("B"), IntNum(2)))))
      )))
    )
    module(mainRelation)
  }
  
  test("Map from"){
    val mainRelation = Relation(
      "main",
      Seq(Param("m", TMap(TString, TInt))),
      Seq(Body(Seq(
        Eq(Var("m"), MapFrom("someCall"))
      )))
    )
    val someCallRelation = Relation(
      "someCall",
      Seq(Param("k", TDemand(TString)), Param("v", TInt)),
      Seq(
        Body(Seq(
          Eq(Var("k"), StringLit("A")),
          Eq(Var("v"), IntNum(1))
        )),
        Body(Seq(
          Eq(Var("k"), StringLit("B")),
          Eq(Var("v"), IntNum(2))
        ))
      )
    )

    module(mainRelation, someCallRelation)
  }
  

  test("Map comprehension"){
    val mainRelation = Relation(
      "main",
      Seq(Param("m2", TMap(TInt, TInt))),
      Seq(Body(Seq(
        Eq(Var("m1"), MapLit(Seq((IntNum(1), IntNum(2))))),
        Eq(Var("m2"), MapComprehension(Var("k"), Add(Var("v"), IntNum(1)), Seq(
          MapContains(Var("m1"), Var("k")),
          Eq(Var("v"), MapLookUp(Var("m1"), Var("k")))
        )))
      )))
    )
    //println(module(mainRelation))

  }
}
