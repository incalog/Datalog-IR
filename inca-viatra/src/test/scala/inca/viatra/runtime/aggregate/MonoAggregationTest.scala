package inca.viatra.runtime.aggregate

import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation1, Relation3, UnitRelation, Relation as Table}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.mono.ArithmeticMonoDefinition.{Sum, Max}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, Var, string2name}
import inca.ir.extension.{aggregate, arithmetic, block, bool, data, demand, impure, mono, string}
import inca.ir.extension.mono.{WriteMono, NewMono, ReadMono, TMono}
import org.scalatest.funsuite.AnyFunSuiteLike


class MonoAggregationTest extends AnyFunSuiteLike {
  private val langs : Language = BaseIR.language +
    arithmetic.IR + 
    demand.IR + 
    data.IR + 
    aggregate.IR +
    mono.IR +
    impure.IR +
    block.IR +
    string.IR +
    bool.IR
  
  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledMonoModule(mod)
    compiledMod.setPipeline(CompiledMonoModule.pipeline)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

  private lazy val relation1: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), NewMono(Sum, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(impure.Hints.Pure)


  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )


  test("Test case 1") {
    val engine = compile(relation1, mainInput)
    engine.insert(Relation1("main$input", Seq("id"), Seq(Seq(1))))
    val res = engine.read(UnitRelation("main"))
    assertResult(0)(res.entries.head)
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
    )))).addHint(impure.Hints.Pure)

  test("Test case 2") {
    val engine = compile(relation3, mainInput)
    engine.insert(Relation1("main$input", Seq("id"), Seq(Seq(1))))
    val res = engine.read(UnitRelation("main"))
    assertResult(1)(res.entries.head)
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
    )))).addHint(impure.Hints.Pure)

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

  test("Test case 3") {
    val engine = compile(relation4, relation5, mainInput, extLeaf)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    val res = engine.read(UnitRelation("main"))
    assertResult(3)(res.entries.head)
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
    )))).addHint(impure.Hints.Pure)

  test("Test case 4") {
    val engine = compile(relation6, relation5, mainInput, extLeaf)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    val res = engine.read(UnitRelation("main"))
    assertResult((3, 0))(res.entries.head)
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


  test("Test case 5") {
    val engine = compile(relation6, relation7, mainInput, extLeaf, extBTree)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    val res = engine.read(UnitRelation("main"))
    assertResult((5, 0))(res.entries.head)
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
    )))).addHint(impure.Hints.Pure)

  test("Test case 6") {
    val engine = compile(relation8, relation7, mainInput, extLeaf, extBTree)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    val res = engine.read(UnitRelation("main"))
    assertResult((5, 1))(res.entries.head)
  }
}
