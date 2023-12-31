package inca.ir.extension.mono


import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, Module, ModuleEntry, Name, Param, Relation, Var, string2name}
import inca.ir.execution.Relation2
import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, TData, IR as dataIR}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.{aggregate, mono, set}
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.extension.set.{SetComprehension, SetIntersection, SetLit, SetMember, SetUnion, TSet}
import inca.ir.extension.string.TString
import inca.ir.extension.tuple.{TTuple, TupleLit}
import inca.ir.extension.{demand, impure}
import inca.ir.visitors.BaseIRVisitor
import org.scalatest.funsuite.AnyFunSuiteLike

class SetMonoOptimizerTest extends AnyFunSuiteLike {
  private val baseIR: BaseIR = new BaseIR {}

  private lazy val pipeline: Seq[() => BaseIRVisitor] = Seq(
    () => new mono.Lowering {},
    () => new impure.Lowering {}, // TODO: fix the bug that optimization cannot be performed before impure lowering
    () => new demand.Lowering {},
    () => new Optimizer {},
    () => new set.Lowering {},
  )

  private val debug: Boolean = false

  def module(relations: ModuleEntry*): Module =
    val mod = Module("M", BaseIR.language + demand.IR + mono.IR + impure.IR + dataIR + set.IR + aggregate.IR, relations)
    println("Original program\n" + mod + "\n\n")
    var lowered = mod
    {
      val typechecker = new IRTypechecker {}
      typechecker.checkProgram(Seq(lowered))
    }
    pipeline.foldLeft(lowered){ case (m, lowering) =>
      val lowFun = lowering()
      println(s"Start lowering ${lowFun.name}")
      lowered = lowFun.visitProgram(Seq(m)).head
      val typechecker = new IRTypechecker {}
      try typechecker.checkProgram(Seq(lowered))
      finally {
        println(s"<><><><><><><> After ${lowFun.name} lowering, program becomes: <><><><><><><>\n" + lowered)
        typechecker.getErrors.foreach(println)
        println("\n\n\n")
      }
      lowered
    }



  test("Test naive set mono: basic test 1"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        Eq(Var("s"), ReadMono(Var("m")))
      )))
    ).addHint(impure.PureHint)

    module(relation)


  test("Test naive set mono: basic test 2"):
    val relation = Relation(
      "main",
      Seq(Param("s", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt))),
        WriteMono(Var("m"), IntNum(1)),
        SetMember(Var("s"), ReadMono(Var("m")))
      )))
    ).addHint(impure.PureHint)

    module(relation)


  test("Test naive set mono: performing set union with mono result"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        Eq(Var("s1"), ReadMono(Var("m"))),
        Eq(Var("s2"), SetUnion(Var("s1"), SetLit(Seq(IntNum(2)))))
      )))
    ).addHint(impure.PureHint)

    module(relation)


  test("Test naive set mono: performing set intersection with mono result"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        WriteMono(Var("m"), IntNum(2), Seq()),
        Eq(Var("s1"), ReadMono(Var("m"))),
        Eq(Var("s2"), SetIntersection(Var("s1"), SetLit(Seq(IntNum(2), IntNum(3)))))
      )))
    ).addHint(impure.PureHint)

    module(relation)


  test("Test naive set mono: performing set comprehension"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        WriteMono(Var("m"), IntNum(2), Seq()),
        WriteMono(Var("m"), IntNum(3), Seq()),
        Eq(Var("s1"), ReadMono(Var("m"))),
        Eq(Var("s2"), SetComprehension(Add(Var("i"), IntNum(1)), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    ).addHint(impure.PureHint)

    module(relation)

  test("Collect edges"):
    val adtDefs: Seq[ModuleEntry] = Seq(
      DataDefinition("TEdge"),
      CaseDefinition("mkEdge", Seq(TString, TString), TData("TEdge"))
    )

    val mainRelation = Relation("main", Seq(Param("s", TSet(TData("TEdge")))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TData("TEdge")), Seq(), Seq())),
      Call("collEdge", Seq(Var("m").arg)),
      Eq(Var("s"), ReadMono(Var("m")))
    )))).addHint(PureHint)

    val collRelation = Relation(
      "collEdge",
      Seq(
        Param("m", TDemand(TMono(TData("TEdge"), TSet(TData("TEdge")), Seq())))
      ),
      Seq(Body(Seq(
        ExtensionalCall("edge", Seq(Var("p").arg, Var("q").arg)),
        WriteMono(Var("m"), Construct("mkEdge", Seq(Var("p"), Var("q"))), Seq())
      )))
    )

    val extEdge: ExtensionalRelation = ExtensionalRelation(
      "edge", Seq(Param("e1", TString), Param("e2", TString))
    )

    val edbEdge: Relation2[Seq[String], Seq[String]] = Relation2(
      "edge",
      Seq("e1", "e2"),
      Seq(
        Seq("6", "2"), Seq("6", "3"), Seq("8", "4"),
        Seq("6", "4"), Seq("9", "5"), Seq("7", "8"),
        Seq("6", "6"), Seq("6", "7"), Seq("8", "9"),
        Seq("2", "9"), Seq("0", "4"), Seq("0", "7"),
        Seq("1", "5"), Seq("5", "9"), Seq("5", "8"),
        Seq("3", "3"), Seq("9", "0"), Seq("1", "0"),
        Seq("2", "5"), Seq("4", "2"), Seq("0", "0"),
        Seq("8", "3"), Seq("9", "3"), Seq("0", "2"),
        Seq("8", "1")
      )
    )

    module(mainRelation +: collRelation +: extEdge +: adtDefs: _*)


  test("Set Mono with boolean element type (type that can be lowered)"):
    val relation = Relation("main", Seq(Param("b", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TBoolean))),
      Eq(Var("b"), BoolTrue),
      WriteMono(Var("m"), Var("b")),
      WriteMono(Var("m"), BoolFalse),
      SetMember(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)

    module(relation)

  test("Set Mono with tuple element type: 1"):
    val relation = Relation("main", Seq(Param("b", TTuple(Seq(TInt, TInt)))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TTuple(Seq(TInt, TInt))))),
      Eq(Var("a"), TupleLit(Seq(IntNum(-1), IntNum(-2)))),
      Eq(Var("b"), TupleLit(Seq(IntNum(1), IntNum(2)))),
      WriteMono(Var("m"), Var("a")),
      WriteMono(Var("m"), Var("b")),
      SetMember(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)


    module(relation)
}
