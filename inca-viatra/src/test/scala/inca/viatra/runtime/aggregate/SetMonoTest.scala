package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.ForeignScalaLowering
import inca.foreign.scala.ir.set.scalaSetMonoDefinition
import inca.ir
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation2, UnitRelation}
import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, TData}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.extension.mono.*
import inca.ir.extension.set.{IR, *}
import inca.ir.extension.string.TString
import inca.ir.extension.tuple.{TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.{block, demand, foreign, impure, map, mono, arithmetic as incaArithmetic, bool as incaBool, data as incaData, set as irSet, string as incaString}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, Var, string2name}
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike


case class CompiledSetMonoModule(mod: Module) extends CompiledModule:
  override def compilerOptions: CompilerOptions = CompilerOptions.default
  override def name: Name = mod.name
  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

  override def ir: Module = mod

  private class ScalaSetTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new ScalaSetTypeChecker

  override def optimize(p: Seq[Module]): Seq[Module] = p

  private trait demandLowering extends demand.Lowering with primitive.Visitor
  private trait blockLowering extends block.Lowering with primitive.Visitor

  setPipeline(List(
    () => new mono.Lowering(optimizeSetMono = false) {},
    () => new primitive.ConversionElimination {},
    () => new impure.Lowering {},
    () => new irSet.Lowering {},
    () => new demandLowering {},
    () => new ForeignScalaLowering {},
    () => new demandLowering {},
    () => new blockLowering {}
  ))


class SetMonoTest extends AnyFunSuiteLike:

  private val langs: Language = BaseIR.language +
    irSet.IR +
    incaArithmetic.IR +
    block.IR +
    foreign.IR +
    mono.IR +
    impure.IR +
    incaData.IR +
    incaString.IR +
    incaBool.IR +
    tupleIR +
    map.IR

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledSetMonoModule(mod)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

  test("Test naive set mono: basic test 1"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Impure.init(IntNum(0), MonoImpurityKind),
        Eq(Var("m"), NewMono(scalaSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        WriteMono(Var("m"), IntNum(17), Seq()),
        Eq(Var("s"), ReadMono(Var("m")))
      )))
    ).addHint(impure.PureHint)

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.size == 1)
//    assertResult(Set(1, 17))(res.entries.head)


  test("Test naive set mono: basic test 2"):
    val relation = Relation(
      "main",
      Seq(Param("s", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("m"), NewMono(scalaSetMonoDefinition(TInt))),
        WriteMono(Var("m"), IntNum(1)),
        WriteMono(Var("m"), IntNum(17)),
        SetMember(Var("s"), ReadMono(Var("m")))
      )))
    ).addHint(impure.PureHint)

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set(1, 17))(res.entries.toSet)


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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 2))(res.entries.head)

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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(2))(res.entries.head)

  test("Test naive set mono: performing set comprehension"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TInt))),
      Seq(Body(Seq(
        Impure.init(IntNum(0), MonoImpurityKind),
        Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TInt), Seq(), Seq())),
        WriteMono(Var("m"), IntNum(1), Seq()),
        WriteMono(Var("m"), IntNum(2), Seq()),
        WriteMono(Var("m"), IntNum(3), Seq()),
        Eq(Var("s1"), ReadMono(Var("m"))),
        Eq(Var("s2"), SetComprehension(Add(Var("i"), IntNum(1)), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    ).addHint(impure.PureHint)

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(2, 3, 4))(res.entries.head)


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

    val engine = compile(mainRelation +: collRelation +: extEdge +: adtDefs:_*)
    engine.insert(edbEdge)
    engine.readAll().foreach(res => println(res.asTable))

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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(true)(res.entries.head)


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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult((1, 2))(res.entries.head)

  test("Set Mono with tuple element type: 2"):
    val relation = Relation("main", Seq(Param("c", TInt), Param("d", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TTuple(Seq(TInt, TInt))))),
      WriteMono(Var("m"), TupleLit(Seq(IntNum(-1), IntNum(-2)))),
      SetMember(TupleLit(Seq(Var("c"), Var("d"))), ReadMono(Var("m")))
    )))).addHint(PureHint)

    // Problem: if tuple is compiled into Scala terms, arguments "c" and "d" cannot be unbound variables
    assertThrows[TypeErrorException] {
      val engine = compile(relation)
    }


  test("Set Mono with recursive relation 1"):
    val mainRelation = Relation("main", Seq(Param("elem", TString)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(TString))),
      Eq(Var("node1"), StringLit("A")),
      Call("collNode", Seq(Var("m").arg, Var("node1").arg)),
      Eq(Var("node2"), StringLit("F")),
      Call("collNode", Seq(Var("m").arg, Var("node2").arg)),
      SetMember(Var("elem"), ReadMono(Var("m")))
    )))).addHint(PureHint)

    val collNode = Relation("collNode",
      Seq(
        Param("mono", TDemand(TMono(TString, TSet(TString), Seq()))),
        Param("t", TDemand(TString))
      ), Seq(Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t").arg)),
        WriteMono(Var("mono"), Var("t"))
      )),
        Body(Seq(
          ExtensionalCall("btree", Seq(Var("t").arg, Var("l").arg, Var("r").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("l").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("r").arg)),
          WriteMono(Var("mono"), Var("t"))
        ))
      ))

    val extLeaf: ExtensionalRelation = ExtensionalRelation(
      "leaf", Seq(Param("t", TString))
    )

    val extBTree: ExtensionalRelation = ExtensionalRelation(
      "btree", Seq(Param("t", TString), Param("l", TString), Param("r", TString))
    )

    lazy val edbLeaf: Relation1[Seq[String]] = Relation1("leaf", Seq("t"), Seq(Seq("C"), Seq("D"), Seq("E"), Seq("I"), Seq("J"), Seq("K"), Seq("L")))

    val edbBTree: Relation3[Seq[String], Seq[String], Seq[String]] = Relation3(
      "btree",
      Seq("t", "l", "r"),
      Seq(
        Seq("A", "B", "C"),
        Seq("B", "D", "E"),
        Seq("F", "G", "H"),
        Seq("G", "I", "K"),
        Seq("H", "L", "J")
      )
    )

    val engine = compile(mainRelation, collNode, extLeaf, extBTree)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set("A", "B", "C", "D", "E", "F", "G", "H", "I", "K", "L", "J"))(res.entries.toSet)


  test("Set Mono with recursive relation 2"):
    val intPair = TTuple(Seq(TInt, TInt))
    val mainRelation = Relation("main", Seq(Param("pair", intPair)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(NaiveSetMonoDefinition(intPair))),
      Call("collPair", Seq(Var("m").arg, IntNum(10).arg)),
      SetMember(Var("pair"), ReadMono(Var("m")))
    )))).addHint(PureHint)

    val evenRelation = Relation(
      "collPair",
      Seq(
        Param("m", TDemand(TMono(intPair, TSet(intPair), Seq()))),
        Param("i", TDemand(TInt))
      ),
      Seq(
        Body(Seq(
          GE(Var("i"), IntNum(2)),
          Call("collPair", Seq(Var("m").arg, Sub(Var("i"), IntNum(2)).arg)),
          WriteMono(Var("m"), TupleLit(Seq(Var("i"), Sub(Var("i"), IntNum(1)))))
        )),
        Body(Seq(
          WriteMono(Var("m"), TupleLit(Seq(Var("i"), Sub(Var("i"), IntNum(1)))))
        ))
      )
    )

    val engine = compile(mainRelation, evenRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((0, -1), (6, 5), (10, 9), (2, 1), (4, 3), (8, 7)))(res.entries.toSet)