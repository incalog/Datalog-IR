package inca.ir.extension.monotypes

import inca.ir.execution.{Relation1, Relation3}
import inca.ir.{Var, *}
import inca.ir.extension.monotypes
import inca.ir.extension.monotypes.ArithmeticMono.{MaxMono, SumMono}
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
//    () => ("impure", new impure.Lowering {}),
//    () => ("demand", new Lowering {})
  )

  private val debug: Boolean = false

  def module(relations: ModuleEntry*): Module =
    val mod = Module("M", BaseIR.language + demand.IR + monotypes.IR + impure.IR + data.IR, relations)
    println("Original program\n" + mod + "\n\n")
    var lowered = mod
    for (phase <- pipeline) {
      val (name, lower) : (String, BaseIRVisitor) = phase()
      lowered = lower.visitProgram(Seq(lowered)).head
      val typechecker = new IRTypechecker {}
      try typechecker.typecheck(lowered)
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
      Eq(Var("m"), MkMono(SumMono, Seq(), Seq(TString))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))).addHint(impure.Hints.Pure)

  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )

  test("Lower Mono Types 1") {
    module(relation1, mainInput)
  }

  private lazy val relation3: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(SumMono, Seq(), Seq(TString))),
      AddMono(Var("m"), IntNum(1), Seq(StringLit("A"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))).addHint(impure.Hints.Pure)

  test("Lower Mono Types 2") {
    implicit val typechecker1 = new IRTypechecker {}
    module(relation3, mainInput)
  }


  private lazy val relation4: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(SumMono, Seq(), Seq(TString))),
      Call("size", Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))).addHint(impure.Hints.Pure)

  private lazy val relation5: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
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


  test("Lower Mono Types 3") {
    module(relation4, relation5, mainInput, extLeaf)
  }


  private lazy val relation6: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m1"), MkMono(SumMono, Seq(), Seq(TString))),
      Eq(Var("m2"), MkMono(SumMono, Seq(), Seq(TString))),
      Call("size", Seq(Var("t"), Var("m1"))),
      Eq(Var("b1"), ResultMono(Var("m1"))),
      Eq(Var("b2"), ResultMono(Var("m2")))
    )))).addHint(impure.Hints.Pure)

  test("Lower Mono Types 4") {
    module(relation6, relation5, mainInput, extLeaf)
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
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      )),
      Body(Seq(
        ExtensionalCall(Name("btree"), Seq(Var("t"), Var("l"), Var("r"))),
        Call(Name("size"), Seq(Var("l"), Var("m"))),
        Call(Name("size"), Seq(Var("r"), Var("m"))),
        AddMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  ).addHint(impure.Hints.Pure)

  test("Test case 5") {
    module(relation6, relation7, mainInput, extLeaf, extBTree)
  }


  private lazy val relation8: Relation = Relation(
    "main",
    Seq(
      Param("b1", TInt),
      Param("b2", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m1"), MkMono(SumMono, Seq(), Seq(TString))),
      Eq(Var("m2"), MkMono(MaxMono, Seq(), Seq(TString))),
      Call("size", Seq(Var("t"), Var("m1"))),
      Call("size", Seq(Var("t"), Var("m2"))),
      Eq(Var("b1"), ResultMono(Var("m1"))),
      Eq(Var("b2"), ResultMono(Var("m2")))
    )))).addHint(impure.Hints.Pure)

  test("Test case 6") {
    module(relation8, relation7, mainInput, extLeaf, extBTree)
  }
}
