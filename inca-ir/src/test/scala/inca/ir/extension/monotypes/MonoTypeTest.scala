package inca.ir.extension.monotypes

import inca.ir.{Var, *}
import inca.ir.extension.monotypes
import inca.ir.extension.monotypes.ArithmeticMono.CountMono
import inca.ir.extension.string
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand
import inca.ir.extension.demand.{TDemand, demandRelationName}
import inca.ir.typing.{BaseIRTypechecker, TypeErrorException, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class MonoTypeTest extends AnyFunSuiteLike {
  val baseIR: BaseIR = new BaseIR {}

  def module(relations: Relation*)(using typechecker: BaseIRTypechecker): Module =
    val mod = Module("M", BaseIR.language, relations)
    try typechecker.typecheck(mod)
    finally {
      println(mod)
      typechecker.getErrors.foreach(println)
    }
    mod

  // main(t, b) :- MkMono(m, MaxMono, Seq(), MT[Int, Int, Seq(String)),
  //               t = "A", size(t, m), b = m.result()
  private lazy val relation1 : Relation = Relation(
    "main",
    Seq(Param("t", TString), Param("b", TInt)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), TMono(TInt, TInt, Seq(TString)))),
      Eq(Var("t"), StringLit("A")),
      Call(Name("size"), Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))
  )

  // size(t, m) :- leaf(t), t += 1@(t)
  //            :- btree(t, l, r), size(l, m), size(r, m),
  //               t += 1@(t)
  private lazy val relation2 : Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TMono(TInt, TInt, Seq(TString)))),
    Seq(
      Body(Seq(
        Call(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      )),
      Body(Seq(
        Call(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )


  // used to emulate ExtensionalCall leaf
  private lazy val relation3 : Relation = Relation(
    "leaf", Seq(Param("t", TString)),
    Seq(
      Body(Seq(Eq(Var("t"), StringLit("C")))),
      Body(Seq(Eq(Var("t"), StringLit("D")))),
      Body(Seq(Eq(Var("t"), StringLit("E"))))
    )
  )

  // used to emulate ExtensionalCall btree
  private lazy val relation4 : Relation = Relation(
    "btree",
    Seq(Param("t", TString), Param("l", TString), Param("r", TString)),
    Seq(
      Body(Seq(Eq(Var("t"), StringLit("A")), Eq(Var("l"), StringLit("B")), Eq(Var("r"), StringLit("C")))),
      Body(Seq(Eq(Var("t"), StringLit("B")), Eq(Var("l"), StringLit("D")), Eq(Var("r"), StringLit("E")))),
    )
  )

  // Change the type of mono in "main" relation to MT[TInt, TString, Seq(TString))
  // would let type checker throws an error
  private lazy val relation1UnSafe: Relation = Relation(
    "main",
    Seq(Param("t", TString), Param("b", TInt)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), TMono(TInt, TString, Seq(TString)))),
      Eq(Var("t"), StringLit("A")),
      Call(Name("size"), Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))
  )

  // Change the type of input of mono in "size" relation to String
  // would let the type checker throws an error
  private lazy val relation2Unsafe: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TMono(TInt, TInt, Seq(TString)))),
    Seq(
      Body(Seq(
        Call(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), StringLit("1"), Seq(Var("t")))
      )),
      Body(Seq(
        Call(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        AddMono(Var("m"), StringLit("1"), Seq(Var("t")))
      ))
    )
  )


  test("Tree size (type-safe)"){
    implicit val typechecker = new BaseIRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker
      {}
    module(relation1, relation2, relation3, relation4)
  }

  test("Tree size (type-unsafe 1)") {
    implicit val typechecker = new BaseIRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(relation1UnSafe, relation2, relation3, relation4)
    )
  }


  test("Tree size (type-unsafe 2)") {
    implicit val typechecker = new BaseIRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(relation1, relation2Unsafe, relation3, relation4)
    )
  }
}
