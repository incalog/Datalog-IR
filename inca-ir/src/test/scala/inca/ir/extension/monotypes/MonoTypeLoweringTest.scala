package inca.ir.extension.monotypes

import inca.ir.{Var, *}
import inca.ir.extension.monotypes
import inca.ir.extension.monotypes.ArithmeticMono.{CountMono, MaxMono}
import inca.ir.extension.string
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.{Lowering, TDemand}
import inca.ir.extension.demand
import inca.ir.extension.impure
import inca.ir.extension.data
import inca.ir.typing.IRTypechecker
import inca.ir.visitors.BaseIRVisitor
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq
class MonoTypeLoweringTest extends AnyFunSuiteLike {
  private val baseIR: BaseIR = new BaseIR {}

  private lazy val pipeline: Seq[() => (String, BaseIRVisitor)] = Seq(
    () => ("monotype", new monotypes.Lowering {}),
    () => ("impure", new impure.Lowering {})
  )

  private val debug: Boolean = false

  def module(relations: ModuleEntry*)(using typechecker: Typechecker): Module =
    val mod = Module("M", BaseIR.language + demand.IR + monotypes.IR + impure.IR + data.IR, relations)
    println("Original program\n" + mod + "\n\n")
    var lowered = mod
    for (phase <- pipeline) {
      val (name, lower) : (String, BaseIRVisitor) = phase()
      lowered = lower.visitProgram(Seq(lowered)).head
      try typechecker.typecheck(lowered)
      finally {
        println(s"After $name lowering, program becomes\n" + lowered)
        typechecker.getErrors.foreach(println)
        println("\n\n\n")
      }
    }
    lowered

  // main(t, b) :- MkMono(m, CountMono, Seq(), MT[Int, Int, Seq(String)),
  //               t = "A", size(t, m), b = m.result()
  private lazy val relation1: Relation = Relation(
    "main",
    Seq(Param("t", TString), Param("b", TInt)),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
      Eq(Var("t"), StringLit("A")),
      Call(Name("size"), Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))
  ).addHint(impure.Hints.Pure)

  // size(t, m) :- leaf(t), t += 1@(t)
  //            :- btree(t, l, r), size(l, m), size(r, m),
  //               t += 1@(t)
  private lazy val relation2: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        Call(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      )),
      Body(Seq(
        Call(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        AddMono(Var("m"), IntNum(2), Seq(Var("t")))
      ))
    )
  )


  // used to emulate ExtensionalCall leaf
  private lazy val relation3: Relation = Relation(
    "leaf", Seq(Param("t", TString)),
    Seq(
      Body(Seq(Eq(Var("t"), StringLit("C")))),
      Body(Seq(Eq(Var("t"), StringLit("D")))),
      Body(Seq(Eq(Var("t"), StringLit("E"))))
    )
  )

  // used to emulate ExtensionalCall btree
  private lazy val relation4: Relation = Relation(
    "btree",
    Seq(Param("t", TString), Param("l", TString), Param("r", TString)),
    Seq(
      Body(Seq(Eq(Var("t"), StringLit("A")), Eq(Var("l"), StringLit("B")), Eq(Var("r"), StringLit("C")))),
      Body(Seq(Eq(Var("t"), StringLit("B")), Eq(Var("l"), StringLit("D")), Eq(Var("r"), StringLit("E")))),
    )
  )


  private lazy val relation6: Relation = Relation(
    "size",
    Seq(Param("t", TDemand(TString)), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        Call(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
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
  ).addHint(impure.Hints.Pure)

  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(Name("main$input"), Seq(Param(Name("MID"), TInt)))

  // main(t, b1, b2) :- MkMono(m1, CountMono, Seq(), MT[Int, Int, Seq(String)),
  //               MkMono(m2, MaxMono, Seq(), MT[Int, Int, Seq(String))
  //               t = "A", size(t, m1), b1 = m1.result(), b2 = m2.result()
  private lazy val relation9: Relation = Relation(
    "main",
    Seq(Param("t", TString), Param("b1", TInt), Param("b2", TInt)),
    Seq(Body(Seq(
      Eq(Var("m1"), MkMono(CountMono, Seq(), Seq(TString))),
      Eq(Var("m2"), MkMono(MaxMono, Seq(), Seq(TString))),
      Eq(Var("t"), StringLit("A")),
      Call(Name("size"), Seq(Var("t"), Var("m1"))),
      Call(Name("size"), Seq(Var("t"), Var("m2"))),
      Eq(Var("b1"), ResultMono(Var("m1"))),
      Eq(Var("b2"), ResultMono(Var("m2")))
    )))
  ).addHint(impure.Hints.Pure)


  test("Lower Mono Types 1") {
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation8, mainInput)
  }

  test("Lower Mono Types 2") {
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation1, relation3, relation6, mainInput)
  }

  test("Lower Mono Types 3") {
    implicit val typechecker1 = new IRTypechecker {}
    val mod = module(relation9, relation2, relation3, relation4, mainInput)
  }


}
