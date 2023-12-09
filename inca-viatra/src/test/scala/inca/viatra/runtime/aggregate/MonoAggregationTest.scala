package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir.primitive.ScalaMonoDefinition
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation1, Relation3, UnitRelation, Relation as Table}
import inca.ir.extension.arithmetic.{IntNum, TDouble, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.extension.mono.ArithmeticMonoDefinition.{Count, CountFrom, MaxInt, SumInt}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.{BaseIR, Body, Call, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, Term, Type, Var, string2name, term2Arg}
import inca.ir.extension.{aggregate, arithmetic, block, bool, data, demand, impure, mono, string}
import inca.ir.extension.mono.{MonoImpurityKind, MonoTypes, NewMono, ReadMono, TMono, WriteMono}
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
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)


  private lazy val mainInput: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )


  test("Test case 1") {
    val engine = compile(relation1)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(0)(res.entries.head)
  }

  private lazy val relation3: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      WriteMono(Var("m"), IntNum(1), Seq(StringLit("A"))),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(impure.PureHint)

  test("Test case 2") {
    val engine = compile(relation3)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(1)(res.entries.head)
  }

  private lazy val relation4: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(SumInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m"))),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(impure.PureHint)

  private lazy val relation5: Relation = Relation(
    "size",
    Seq(Param("t", TString), Param("m", TDemand(TMono(TInt, TInt, Seq(TString))))),
    Seq(
      Body(Seq(
        ExtensionalCall(Name("leaf"), Seq(Var("t"))),
        WriteMono(Var("m"), IntNum(1), Seq(Var("t")))
      ))
    )
  )

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
    val engine = compile(relation4, relation5, extLeaf)
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
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m1"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(SumInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    )))).addHint(PureHint)

  test("Test case 4") {
    val engine = compile(relation6, relation5, extLeaf)
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
  )


  test("Test case 5") {
    val engine = compile(relation6, relation7, extLeaf, extBTree)
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
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m1"), NewMono(SumInt, Seq(TString), Seq())),
      Eq(Var("m2"), NewMono(MaxInt, Seq(TString), Seq())),
      Call("size", Seq(Var("t"), Var("m1"))),
      Call("size", Seq(Var("t"), Var("m2"))),
      Eq(Var("b1"), ReadMono(Var("m1"))),
      Eq(Var("b2"), ReadMono(Var("m2")))
    )))).addHint(impure.PureHint)

  test("Test case 6") {
    val engine = compile(relation8, relation7, extLeaf, extBTree)
    engine.insert(edbMainInput)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    val res = engine.read(UnitRelation("main"))
    assertResult((5, 1))(res.entries.head)
  }

  private val customMono = ScalaMonoDefinition(
    Name("addString"),
    "0.0", // we should be able to typecheck the foreign scala term, e.g. report errors if it was 0.0
    "(st: Double, a: Int) => st + a",
    "(st: Double) => st.toString",
    Seq(),
    MonoTypes(TInt, TDouble, TString)
  )

  private lazy val relationUserDefinedMono: Relation = Relation(
    "main",
    Seq(
      Param("b", TString)
    ),
    Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Var("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("m"), NewMono(customMono, Seq(TString), Seq())),
      Eq(Var("b"), ReadMono(Var("m")))
    )))).addHint(PureHint)

  test("Test using user-defined mono definition 1") {
    val engine = compile(relationUserDefinedMono)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult("0.0")(res.entries.head)
  }
}
