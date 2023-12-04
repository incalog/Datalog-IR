package inca.ir.extension.map

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.typing.IRTypechecker
import inca.ir.{Body, Eq, Language, Module, Param, Relation, TNothing, Var, string2name}
import org.scalatest.funsuite.AnyFunSuiteLike


class MapTest extends AnyFunSuiteLike {
  def module(relations: Relation*): Module =
    val typechecker = new IRTypechecker {}
    val mod = Module("M", Language(IR), relations)
    typechecker.checkProgram(Seq(mod))
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
}
