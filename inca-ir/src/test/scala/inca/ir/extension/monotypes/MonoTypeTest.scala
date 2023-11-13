package inca.ir.extension.monotypes

import inca.ir.extension.aggregate.Aggregate
import inca.ir.{Var, *}
import inca.ir.extension.monotypes
import inca.ir.extension.monotypes.ArithmeticMono.CountMono
import inca.ir.extension.string
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.{Lowering, TDemand}
import inca.ir.extension.demand
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{Arg, AggregateColumn, WildCard}
import inca.ir.typing.{IRTypechecker, TypeErrorException}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class MonoTypeTest extends AnyFunSuiteLike {
  val baseIR: BaseIR = new BaseIR {}

  def module(relations: Relation*)(using typechecker: Typechecker): Module =
    val mod = Module("M", BaseIR.language+demand.IR+monotypes.IR, relations)
    try typechecker.typecheck(mod)
    finally {
//      println(mod)
      typechecker.getErrors.foreach(println)
    }
    mod

  // main(t, b) :- MkMono(m, MaxMono, Seq(), MT[Int, Int, Seq(String)),
  //               t = "A", size(t, m), b = m.result()
  private lazy val relation1 : Relation = Relation(
    "main",
    Seq(Param("t", TString), Param("b", TInt)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
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
    Seq(Param("t", TDemand(TString)), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
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
    Seq(Param("t", TString), Param("b", TString)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
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
    implicit val typechecker = new IRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker
      {}
    module(relation1, relation2, relation3, relation4)
  }

  test("Tree size (type-unsafe 1)") {
    implicit val typechecker = new IRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(relation1UnSafe, relation2, relation3, relation4)
    )
  }


  test("Tree size (type-unsafe 2)") {
    implicit val typechecker = new IRTypechecker
      with monotypes.Typechecker
      with string.Typechecker
      with arithmetic.Typechecker {}
    assertThrows[TypeErrorException](
      module(relation1, relation2Unsafe, relation3, relation4)
    )
  }

  private lazy val relation6: Relation = Relation(
    "size",
    Seq(Param("t", TDemand(TString)), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )

  test("Demand transformation"){
    implicit val typechecker = new IRTypechecker {}
    val lowering = new Lowering {}
    val mod = module(relation3, relation6)
    val lowered = lowering.visitProgram(Seq(mod)).head
    val typeckecker2 = new IRTypechecker {}
    typeckecker2.typecheck(lowered)
    println(lowered)
  }

  private lazy val relation7: Relation = Relation(
    "main",
    Seq(Param("t", TString)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
      Eq(Var("t"), StringLit("A")),
      Call(Name("size"), Seq(Var("t"), Var("m"))),
//      Eq(Var("b"), ResultMono(Var("m")))
    )))
  )

  private lazy val relation8: Relation = Relation(
    "main",
    Seq(
      Param("t", TString),
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
      Eq(Var("t"), StringLit("A")),
//      Call(Name("size"), Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))
  )


  test("Lower Mono Types 1") {
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation8)
    val lowering1 = new monotypes.Lowering {}
    val lowered1 = lowering1.visitProgram(Seq(mod)).head
    val typeckecker2 = new IRTypechecker {}
    typeckecker2.typecheck(lowered1)
  }

  test("Lower Mono Types 2"){
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation1, relation3, relation6)
    val lowering1 = new monotypes.Lowering {}
    val lowered1 = lowering1.visitProgram(Seq(mod)).head
    val typeckecker2 = new IRTypechecker {}
    typeckecker2.typecheck(lowered1)
  }

  test("Lower Mono Types 3") {
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation1, relation2, relation3, relation4)
    val lowering1 = new monotypes.Lowering {}
    val lowered1 = lowering1.visitProgram(Seq(mod)).head
    val typeckecker2 = new IRTypechecker {}
    typeckecker2.typecheck(lowered1)
  }


  private lazy val relation9: Relation = Relation(
    Name("Coll"),
    Seq(Param(Name("m"), TString), Param(Name("k"), TString), Param(Name("t"), TInt)),
    Seq(Body(Seq(
      Eq(Var("m"), StringLit("m")),
      Eq(Var("k"), StringLit("t")),
      Eq(Var("t"), IntNum(1))
    )))
  )

  private lazy val relation10: Relation = Relation(
    Name("Agg"),
    Seq(Param(Name("m"), TString), Param(Name("k"), TString), Param(Name("t"), TInt)),
    Seq(Body(Seq(
      Call(Name("Coll"), Seq(Var("m"), Var("k"), Var("t")))
    )))
  )

  private lazy val relation11: Relation = Relation(
    Name("main"),
    Seq(Param(Name("t"), TInt)),
    Seq(Body(Seq(
      Aggregate(Name("Agg"), Seq(WildCard, WildCard,
        AggregateColumn(Var("t"))), CountMono)
    )))
  )

  test("aggregate type checking"){
    implicit val typechecker = new IRTypechecker {}
    val m1 = Module("M", BaseIR.language + aggregate.IR, Seq(relation9, relation10, relation11))
    println(m1)
    val mod = module(relation9, relation10, relation11)
    print(mod)
  }

}
