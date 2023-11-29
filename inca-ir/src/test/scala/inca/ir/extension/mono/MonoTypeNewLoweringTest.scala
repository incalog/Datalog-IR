package inca.ir.extension.mono

import inca.ir.execution.{Relation1, Relation3}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.{Lowering, TDemand}
import inca.ir.extension.mono.ArithmeticMonoDefinition.{CountFrom, Max, Sum, SumToPair}
import inca.ir.extension.*
import inca.ir.extension.impure.Hints.Pure
import inca.ir.extension.impure.Impure
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.TTuple
import inca.ir.typing.IRTypechecker
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Var, *}
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class MonoTypeNewLoweringTest extends AnyFunSuiteLike {
  private val baseIR: BaseIR = new BaseIR {}

  private lazy val pipeline: Seq[() => (String, BaseIRVisitor)] = Seq(
    () => ("monotype", new mono.NewLowering {}),
    () => ("impure", new impure.Lowering {}),
    () => ("demand", new Lowering {})
  )

  private val debug: Boolean = false

  def module(relations: ModuleEntry*): Module =
    val mod = Module("M", BaseIR.language + demand.IR + mono.IR + impure.IR + data.IR, relations)
    println("Original program\n" + mod + "\n\n")
    var lowered = mod
    {
      val typechecker = new IRTypechecker {}
      typechecker.checkProgram(Seq(lowered))
    }
    for (phase <- pipeline) {
      val (name, lower) : (String, BaseIRVisitor) = phase()
      lowered = lower.visitProgram(Seq(lowered)).head
      val typechecker = new IRTypechecker {}
      try typechecker.checkModule(lowered)
      finally {
        println(s"<><><><><><><> After $name lowering, program becomes: <><><><><><><>\n" + lowered)
        typechecker.getErrors.foreach(println)
        println("\n\n\n")
      }
    }
    lowered

  private lazy val relation1: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(Sum, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    ))))

  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )

  test("Lower Mono Types: nothing is added into a mono") {
    module(relation1)
  }

  private lazy val relationCountFrom: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), NewMono(CountFrom, Seq(TString), Seq(IntNum(5)))),
      Eq(Var("b"), ReadMono(Var("m")))
    ))))

  private lazy val mainInputCountFrom: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )

  test("Lower Mono Types: a mono with arguments") {
    module(relationCountFrom)
  }

  private lazy val relationSumToString: Relation = Relation(
    "main",
    Seq(
      Param("b", TTuple(Seq(TInt, TString)))
    ),
    Seq(Body(Seq(
      Eq(Var("m"), NewMono(SumToPair, Seq(TString), Seq())),
      WriteMono(Var("m"), IntNum(1), Seq(StringLit("A"))),
      Eq(Var("b"), ReadMono(Var("m")))
    ))))

  private lazy val mainInputSumToString: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )

  test("Lower Mono Types: types of state and output are different") {
    module(relationSumToString)
  }

  private lazy val relation3: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), NewMono(Sum, Seq(TString), Seq())),
      WriteMono(Var("m"), IntNum(1), Seq(StringLit("A"))),
      Eq(Var("b"), ReadMono(Var("m")))
    ))))

  test("Lower Mono Types: writing mono and read mono happen in the same body") {
    implicit val typechecker1 = new IRTypechecker {}
    module(relation3)
  }


  private lazy val relation4: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), NewMono(Sum, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ReadMono(Var("m")))
    ))))

  private lazy val relation5: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  ).addHint(impure.Hints.Pure)

  private lazy val extLeaf: ExtensionalRelation = ExtensionalRelation(
    "leaf", Seq(Param("t", TString))
  )

  private lazy val edbMainInput: Relation1[Seq[Int]] = Relation1("main$input", Seq("id"), Seq(Seq(1)))

  private lazy val edbLeaf: Relation1[Seq[String]] = Relation1("leaf", Seq("t"), Seq(Seq("C"), Seq("D"), Seq("E")))

  private lazy val edbBTree: Relation3[Seq[String], Seq[String], Seq[String]] = Relation3(
    "btree",
    Seq("t", "l", "r"),
    Seq(Seq("A", "B", "C"), Seq("B", "D", "E"))
  )


  test("Lower Mono Types: Mono creation and writing happen in different relations") {
    module(relation4, relation5, extLeaf)
  }


  private lazy val relation6: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m1"), NewMono(Sum, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(Sum, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    ))))

  test("Lower Mono Types: create two mono objects having the same MonoOperator") {
    module(relation6, relation5, extLeaf)
  }


  private lazy val extBTree: ExtensionalRelation = ExtensionalRelation(
    "btree", Seq(Param("t", TString), Param("l", TString), Param("r", TString))
  )

  private lazy val relation7: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      )),
      Body(Seq(
        ExtensionalCall(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  ).addHint(impure.Hints.Pure)

  test("Lower Mono: compute the size of tree") {
    module(relation6, relation7, extLeaf, extBTree)
  }


  private lazy val relation8: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m1"), NewMono(Sum, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(Max, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Call("size", Seq(Var("t"), Var("m2"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    ))))

  test("Lower mono: create two monos that have different MonoOperators") {
    module(relation8, relation7, extLeaf, extBTree)
  }


  private lazy val relation9: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        Eq(Var("m1"), NewMono(Sum, Seq(TString), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )

  test("Lower mono: create mono in multiple relations") {
    module(relation4, relation9, extLeaf)
  }



}
