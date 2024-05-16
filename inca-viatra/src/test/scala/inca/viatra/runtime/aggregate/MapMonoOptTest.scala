package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir as scalaExt
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaMonoDefinition, ScalaType}
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation1, Relation3, UnitRelation}
import inca.ir.extension.arithmetic.{Add, DoubleNum, GT, IntNum, Max, Mul, Sub, TDouble, TInt}
import inca.ir.extension.block.Block
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.impure.{Impure, MainHint}
import inca.ir.{BaseIR, Body, Call, Cast, CompiledModule, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, TAny, Term, Type, Var, WildcardArg, string2name}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{ArithmeticMonoDefinition, DisjMonoDefinition, MapMonoDefinition, MonoImpurityKind, MonoTypes, NewMono, ReadMono, SetMonoDefinition, TMono, WriteMono}
import inca.ir.extension.set.{SetLit, SetMember, SetUnion, TSet, IR as setIR, Lowering as setLowering}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.{arithmetic, block, bool, data, demand, map, not, set, string, tuple}
import inca.ir.extension.{disjunction, impure, mono}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.backend
import inca.viatra.backend.Executor
import org.eclipse.viatra.query.runtime.matchers.backend.IQueryBackendFactory
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuiteLike


case class CompiledScalaMapMonoOptModule(mod: Module) extends CompiledModule:
  override def compilerOptions: CompilerOptions =
    val op = CompilerOptions.default
    op.irLogging.logModule = false
    op.irLogging.logLowerings = false
    op

  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

  override def ir: Module = mod

  private class ScalaTypeChecker extends IRTypechecker with scalaExt.primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new ScalaTypeChecker

  // As we introduced foreign term during set lowering, so we cannot
  override def optimize(p: Seq[Module]): Seq[Module] = p

  private trait demandLowering extends demand.Lowering with scalaExt.primitive.Visitor

  private trait blockLowering extends block.Lowering with scalaExt.primitive.Visitor

  private trait disjunctionLowering extends disjunction.Lowering with scalaExt.primitive.Visitor

  private trait tupleLowering extends tuple.Lowering with primitive.Visitor

  private trait mapLowering extends map.Lowering with primitive.Visitor

  private trait setLowering extends set.Lowering with primitive.Visitor

  private trait conversionElimination extends ConversionElimination with primitive.Visitor

  setPipeline(List(
    () => new mono.Lowering(optimizeMono = true) {},
    () => new MonoScalaLowering {},
    () => new conversionElimination {},
    () => new impure.Lowering {},
    () => new setLowering {},
    () => new mapLowering {},
    () => new bool.Lowering {},
    () => new not.Lowering {},
    () => new disjunction.Lowering {},
    () => new blockLowering {},
    () => new demandLowering {},
    () => new tupleLowering {},
    () => new ForeignScalaLowering {},
    () => new blockLowering {},
    () => new disjunctionLowering {},
    () => new demandLowering {},
    () => new blockLowering {}
  ))



class ScalaMapMonoOptTest extends AnyFunSuiteLike:

  private val langs: Language = BaseIR.language +
    set.IR +
    arithmetic.IR +
    block.IR +
    mono.IR +
    impure.IR +
    data.IR +
    string.IR +
    bool.IR +
    tupleIR +
    map.IR +
    disjunction.IR +
    string.IR

  private def compile(backendFactory: IQueryBackendFactory, relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledScalaMapMonoOptModule(mod)
//    val exec: IRExecutor = new inca.viatra.Executor(backendFactory)
    val exec: IRExecutor = new Executor(DRedReteBackendFactory.INSTANCE)
    exec.instantiate(compiledMod)

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod

  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledScalaMapMonoOptModule(mod)
    val exec: IRExecutor = backend.Executor()
    exec.instantiate(compiledMod)
  
  // mono = new MapMono[Int, Int](arithMonoDef)
  // Optimize map mono:
  // m@MapMono += (1, 2)
  // ==> m1@arithMono += 2@1
  // res = m@MapMono.get ==> relation doing aggregation on the arithmetic monos

  test("Map Mono basic test 1: no write mono"):
    val mainRelation = Relation("main", Seq(Param("p", TMap(TInt, TInt))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, ArithmeticMonoDefinition.SumInt))),
      Eq(Var("p"), ReadMono(Var("mono")))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)


  test("Map Mono basic test 2: write mono with primitive keys and built in non-relative value mono"):
    val mainRelation = Relation("main",
      Seq(Param("value", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, ArithmeticMonoDefinition.SumInt))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(1)))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(2)))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        Eq(Var("value"), MapLookUp(Var("map"), IntNum(1)))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(3)(res.entries.head)

  test("Map Mono basic test 3: value mono is set mono with primitive elements"):
    val mainRelation = Relation("main",
      Seq(Param("elem", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, SetMonoDefinition(TInt)))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(1)))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(2)))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(3)))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(2), IntNum(4)))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("elem"), MapLookUp(Var("map"), IntNum(1)))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 2, 3))(res.entries.toSet)


  test("Map Mono basic test 4: value mono is set mono with tuples as elements"):
    val mainRelation = Relation("main",
      Seq(Param("elem", TTuple(Seq(TInt, TString)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), TupleLit(Seq(IntNum(1), StringLit("1")))))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), TupleLit(Seq(IntNum(2), StringLit("2")))))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(2), TupleLit(Seq(IntNum(1), StringLit("3")))))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(2), TupleLit(Seq(IntNum(4), StringLit("4")))))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("elem"), MapLookUp(Var("map"), IntNum(1)))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((1, "1"), (2, "2")))(res.entries.toSet)


  test("Map Mono basic test 5: value mono is a disjunction (boolean) mono"):
    val mainRelation = Relation("main",
      Seq(Param("value", TBoolean)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, DisjMonoDefinition()))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), BoolFalse))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), BoolTrue))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(2), BoolFalse))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        Eq(Var("value"), MapLookUp(Var("map"), IntNum(1)))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(1)(res.entries.head)


  test("Map Mono basic test 6: key is boolean value"):
    val mainRelation = Relation("main",
      Seq(Param("elem", TTuple(Seq(TInt, TString)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TBoolean, SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
        WriteMono(Var("mono"), TupleLit(Seq(BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1")))))),
        WriteMono(Var("mono"), TupleLit(Seq(BoolTrue, TupleLit(Seq(IntNum(2), StringLit("2")))))),
        WriteMono(Var("mono"), TupleLit(Seq(BoolFalse, TupleLit(Seq(IntNum(1), StringLit("3")))))),
        WriteMono(Var("mono"), TupleLit(Seq(BoolFalse, TupleLit(Seq(IntNum(4), StringLit("4")))))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("elem"), MapLookUp(Var("map"), BoolTrue))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((1, "1"), (2, "2")))(res.entries.toSet)


  test("Map Mono: basic test 7: key is of type TTuple[TInt, TString], value mono is SetMono[TTuple[TInt, TInt]]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TTuple(Seq(TInt, TInt)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TInt, TString)), SetMonoDefinition(TTuple(Seq(TInt, TInt)))))),
        WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(IntNum(1), StringLit("1"))), TupleLit(Seq(IntNum(1), IntNum(1)))))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), TupleLit(Seq(IntNum(1), StringLit("1")))))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult((1, 1))(res.entries.head)


  test("Map Mono basic test 8: key is of type TTuple[TInt, TInt], value mono is SetMono[TTuple[TInt, TInt]]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TTuple(Seq(TInt, TInt)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TInt, TInt)), SetMonoDefinition(TTuple(Seq(TInt, TInt)))))),
        WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(IntNum(1), IntNum(1))), TupleLit(Seq(IntNum(1), IntNum(1)))))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), TupleLit(Seq(IntNum(1), IntNum(1)))))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult((1, 1))(res.entries.head)



  test("Map Mono basic test 9: key is of type TTuple[TInt, TString], value mono is SetMono[TInt]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TInt, TString)), SetMonoDefinition(TInt)))),
        WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1)))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), TupleLit(Seq(IntNum(1), StringLit("1")))))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(1)(res.entries.head)



  test("Map Mono basic test 10: key is of type TTuple[TInt, TInt], value mono is SetMono[TInt]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TInt, TInt)), SetMonoDefinition(TInt)))),
        WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(IntNum(1), IntNum(1))), IntNum(1)))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), TupleLit(Seq(IntNum(1), IntNum(1)))))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(1)(res.entries.head)


  test("Map Mono basic test 11: key is of type TString, value mono is SetMono[TInt]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TInt)),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TString, SetMonoDefinition(TInt)))),
        WriteMono(Var("mono"), TupleLit(Seq(StringLit("1"), IntNum(1)))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), StringLit("1")))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(1)(res.entries.head)

  test("Map Mono basic test 12: key is of type TInt, value mono is SetMono[TTuple[TString, TInt]]"):
    val mainRelation = Relation("main",
      Seq(Param("value", TTuple(Seq(TString, TInt)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, SetMonoDefinition(TTuple(Seq(TString, TInt)))))),
        WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), TupleLit(Seq(StringLit("1"), IntNum(1)))))),
        Eq(Var("map"), ReadMono(Var("mono"))),
        SetMember(Var("value"), MapLookUp(Var("map"), IntNum(1)))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(("1", 1))(res.entries.head)

  test("Projecting nested tuples"):
    val mainRelation = Relation("main",
      Seq(
        Param("a", TTuple(Seq(TTuple(Seq(TInt, TString)), TInt))),
        Param("b", TTuple(Seq(TInt, TString))),
        Param("c", TInt),
        Param("d", TString),
        Param("e", TInt)
      ),
      Seq(Body(Seq(
        Eq(Var("a"), TupleLit(Seq(TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1)))),
        Eq(Var("b"), Project(Var("a"), 0)),
        Eq(Var("c"), Project(Var("b"), 0)),
        Eq(Var("d"), Project(Var("b"), 1)),
        Eq(Var("e"), Project(Var("a"), 1)),
      )))
    )

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult((1, "1", 1, 1, "1", 1, "1", 1))(res.entries.head)


  test("Map Mono basic test 13: two map monos whose key type is TTuple[TString, TInt] and value mono is SetMono[TTuple[TInt, TString]]"):
    val mainRelation = Relation("main",
      Seq(Param("elem", TTuple(Seq(TInt, TString)))),
      Seq(Body(Seq(
        Eq(Var("counter"), IntNum(0)),
        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
        Eq(Var("mono1"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
        Eq(Var("mono2"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
        WriteMono(Var("mono1"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))), TupleLit(Seq(IntNum(1), StringLit("1")))))),
        WriteMono(Var("mono1"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))), TupleLit(Seq(IntNum(-1), StringLit("-1")))))),
        WriteMono(Var("mono2"), TupleLit(Seq(TupleLit(Seq(StringLit("-2"), IntNum(2))), TupleLit(Seq(IntNum(2), StringLit("2")))))),
        WriteMono(Var("mono2"), TupleLit(Seq(TupleLit(Seq(StringLit("-2"), IntNum(2))), TupleLit(Seq(IntNum(-2), StringLit("-2")))))),
        Eq(Var("map"), ReadMono(Var("mono2"))),
        SetMember(Var("elem"), MapLookUp(Var("map"), TupleLit(Seq(StringLit("-2"), IntNum(2)))))
      )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((2, "2"), (-2, "-2")))(res.entries.toSet)


  test("Map Mono basic test 14: value mono is a user-defined mono"):
    val SetSizeMono = ScalaMonoDefinition(
      "SetSizeMono",
      initCode = "Set[Any]()",
      addCode = "(st: Set[Any], a: Any) => st + a",
      resultCode = "(st: Set[Any]) => st.size",
      combineCode = s"(o1: Int, o2: Int) => o1 + o2",
      constructorParamTypes = Seq(),
      typ = MonoTypes(ScalaType("Any"), ScalaType("Set[Any]"), ScalaType.int)
    )

    val mainRelation = Relation("main", Seq(Param("size", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetSizeMono))),
      WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))),
        ConvertIRForeign(TupleLit(Seq(IntNum(1), StringLit("1"))), TTuple(Seq(TInt, TString)), ScalaType.any)
      ))),
      WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))),
        ConvertIRForeign(TupleLit(Seq(IntNum(-1), StringLit("-1"))), TTuple(Seq(TInt, TString)), ScalaType.any)
      ))),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("size"), Cast(
        MapLookUp(Var("map"), TupleLit(Seq(StringLit("-1"), IntNum(1)))),
        TInt
      ))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(2)(res.entries.head)


  test("Map mono basic test 15: calculating the height of tree (recursive relation)"):
    val mainRelationSucc = Relation("main", Seq(Param("num", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TString, SumInt))),
      Eq(Var("node"), StringLit("B")),
      Call("collNode", Seq(Var("mono").arg, Var("node").arg)),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("num"), MapLookUp(Var("map"), StringLit("B")))
    )))).addHint(MainHint)

    val mainRelationFail = Relation("main", Seq(Param("num", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TString, SumInt))),
      Eq(Var("node"), StringLit("A")),
      Call("collNode", Seq(Var("mono").arg, Var("node").arg)),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("num"), MapLookUp(Var("map"), StringLit("A")))
    )))).addHint(MainHint)


    val collNode = Relation("collNode",
      Seq(
        Param("mono", TDemand(TMono(TTuple(Seq(TString, TInt)), TMap(TString, TInt), Seq()))),
        Param("t", TDemand(TString))
      ), Seq(Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t").arg)),
        WriteMono(Var("mono"), TupleLit(Seq(Var("t"), IntNum(1))))
      )),
        Body(Seq(
          ExtensionalCall("btree", Seq(Var("t").arg, Var("l").arg, Var("r").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("l").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("r").arg)),
          Eq(Var("map"), ReadMono(Var("mono"))),
          Eq(Var("lh"), MapLookUp(Var("map"), Var("l"))),
          Eq(Var("rh"), MapLookUp(Var("map"), Var("r"))),
          Eq(Var("height"), Add(Max(Var("lh"), Var("rh")), IntNum(1))),
          WriteMono(Var("mono"), TupleLit(Seq(Var("t"), Var("height"))))
        ))
      ))

    val extLeaf = ExtensionalRelation(
      "leaf", Seq(Param("t", TString))
    )

    val extBTree = ExtensionalRelation(
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

    val engine = compile(DRedReteBackendFactory.INSTANCE, mainRelationSucc, collNode, extLeaf, extBTree)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(2)(res.entries.head)


  test("Test generating nested maps from relation"):
    val mainRelation = Relation("main", Seq(
        Param("v1", TInt),
        Param("v2", TInt),
        Param("v3", TInt),
      ), Seq(Body(Seq(Eq(
        Var("map"),
        MapComprehension(
          Var("k1"),
          Var("map1"),
          Seq(
            Call("someRel", Seq(Var("k1").arg, WildcardArg(), WildcardArg())),
            Eq(Var("map1"), MapComprehension(
              Var("k2"),
              Var("v"),
              Seq(Call("someRel", Seq(Var("k1").arg, Var("k2").arg, Var("v").arg)))
            )
          ))
        )
      ),
      Eq(Var("v1"), MapLookUp(MapLookUp(Var("map"), IntNum(1)), IntNum(2))),
      Eq(Var("v2"), MapLookUp(MapLookUp(Var("map"), IntNum(1)), IntNum(3))),
      Eq(Var("v3"), MapLookUp(MapLookUp(Var("map"), IntNum(2)), IntNum(2))),
    ))))

    val mainRelation2 = Relation(
      "main",
      Seq(
        Param("v1", TInt),
        Param("v2", TInt),
        Param("v3", TInt),
        Param("map", TMap(TInt, TMap(TInt, TInt)))
      ), Seq(Body(Seq(
        Eq(Var("map"), MapFun(Seq(Param("k1", TInt)), MapFun(Seq(Param("k2", TInt)), Block(Call("someRel", Seq(Var("k1").arg, Var("k2").arg, Var("v").arg)), Var("v"))))),
        Eq(Var("v1"), MapLookUp(MapLookUp(Var("map"), IntNum(1)), IntNum(3))),
        Eq(Var("v2"), MapLookUp(MapLookUp(Var("map"), IntNum(1)), IntNum(2))),
        Eq(Var("v3"), MapLookUp(MapLookUp(Var("map"), IntNum(2)), IntNum(2))),
      ))
    ))


    val mapRelation = Relation("someRel", Seq(
      Param("k1", TInt),
      Param("k2", TInt),
      Param("v", TInt)
    ), Seq(
      Body(Seq(
        Eq(Var("k1"), IntNum(1)),
        Eq(Var("k2"), IntNum(2)),
        Eq(Var("v"), IntNum(3))
      )),
      Body(Seq(
        Eq(Var("k1"), IntNum(1)),
        Eq(Var("k2"), IntNum(3)),
        Eq(Var("v"), IntNum(4))
      )),
      Body(Seq(
        Eq(Var("k1"), IntNum(2)),
        Eq(Var("k2"), IntNum(2)),
        Eq(Var("v"), IntNum(3))
      ))
    ))
    val engine = compile(mainRelation2, mapRelation)
    //engine.readAll().foreach(res => println(res.asTable))

  test("Map momo basic test 16: value mono is another map mono with a non-relative value mono"):
    val valueMapMono = MapMonoDefinition(TString, SumInt)
    val mapMono = MapMonoDefinition(TString, valueMapMono)
    val mainRelation = Relation("main", Seq(Param("elem", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("pos"), TupleLit(Seq(StringLit("even"), IntNum(2))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("neg"), TupleLit(Seq(StringLit("odd"), IntNum(-1))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("pos"), TupleLit(Seq(StringLit("even"), IntNum(4))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("neg"), TupleLit(Seq(StringLit("even"), IntNum(-2))))
      )),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("elem"), MapLookUp(MapLookUp(Var("map"), StringLit("pos")), StringLit("even")))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(6)(res.entries.head)


  test("Map momo basic test 17: value mono is a multi-map mono"):
    val valueMapMono = MapMonoDefinition(TString, SetMonoDefinition(TInt))
    val mapMono = MapMonoDefinition(TString, valueMapMono)
    val mainRelation = Relation("main", Seq(Param("elem", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("pos"), TupleLit(Seq(StringLit("even"), IntNum(2))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("neg"), TupleLit(Seq(StringLit("odd"), IntNum(-1))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("pos"), TupleLit(Seq(StringLit("even"), IntNum(4))))
      )),
      WriteMono(Var("mono"), TupleLit(
        Seq(StringLit("neg"), TupleLit(Seq(StringLit("even"), IntNum(-2))))
      )),
      Eq(Var("map"), ReadMono(Var("mono"))),
      SetMember(Var("elem"), MapLookUp(MapLookUp(Var("map"), StringLit("pos")), StringLit("even")))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set(2, 4))(res.entries.toSet)


  private def makeTp[T](K: Seq[T] => T, ts: T*): T =
    if ts.size == 1 then ts.head
    else if ts.size == 2 then K(ts.toSeq)
    else K(Seq(ts.head, makeTp(K, ts.tail:_*)))

  private def nmapLookUp(map: Term, keys: Term*): Term =
    if keys.size == 1 then MapLookUp(map, keys.head)
    else if keys.size > 1 then MapLookUp(nmapLookUp(map, keys.dropRight(1):_*), keys.last)
    else throw IllegalAccessError(s"$keys is an empty list")


  test("Map mono basic test 18: 3-level nested map mono with primitive key types"):
    val mapMono = MapMonoDefinition(TInt, MapMonoDefinition(TDouble, MapMonoDefinition(TString, SumInt)))
    val mainRelation = Relation("main", Seq(Param("year", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(2.5), StringLit("JGU"), IntNum(2023))),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(2.5), StringLit("JGU"), IntNum(1))),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(2.5), StringLit("JGU is named at"), IntNum(1946))),
      Eq(Var("thisyear"), nmapLookUp(ReadMono(Var("mono")), IntNum(1), DoubleNum(2.5), StringLit("JGU"))),
      Eq(Var("startedyear"), nmapLookUp(ReadMono(Var("mono")), IntNum(1), DoubleNum(2.5), StringLit("JGU is named at"))),
      Eq(Var("year"), Sub(Var("thisyear"), Var("startedyear"))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(2024-1946)(res.entries.head)
    engine.readAll().map(_.asTable).foreach(println)


  test("Map mono basic test 19: 2-level map mono with non-primitive key types"):
    val mapMono = MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TInt, DisjMonoDefinition()))
    val mainRelation = Relation("main", Seq(Param("res", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1), BoolFalse)),
      Eq(Var("res"), nmapLookUp(ReadMono(Var("mono")), TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1)))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(0)(res.entries.head)


  test("Map mono basic test 20: 3-level nested map mono with non-primitive key types (also test lowering ADT with non-primitive parameters)"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TInt), DisjMonoDefinition())))
    val mainRelation = Relation("main", Seq(Param("res1", TBoolean), Param("res2", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolTrue)),
      Eq(Var("res1"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)))),
      Eq(Var("res2"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult((0, 1))(res.entries.head)

  test("Map mono basic test 21: 3-level nested map mono with non-primitive key types (also test lowering ADT with non-primitive parameters)"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TTuple(Seq(TInt, TString))), DisjMonoDefinition())))
    val mainRelation = Relation("main", Seq(Param("res1", TBoolean), Param("res2", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolFalse)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolTrue)),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolFalse)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("2")))), BoolTrue)),
      Eq(Var("res1"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
      Eq(Var("res2"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult((1, 0))(res.entries.head)


  test("Map mono basic test 22: 3-level nested map mono with user-defined mono definitions"):
    val SetSizeMono = ScalaMonoDefinition(
      "SetSizeMono",
      initCode = "Set[Any]()",
      addCode = "(st: Set[Any], a: Any) => st + a",
      resultCode = "(st: Set[Any]) => st.size",
      combineCode = s"(o1: Int, o2: Int) => o1 + o2",
      constructorParamTypes = Seq(),
      typ = MonoTypes(ScalaType("Any"), ScalaType("Set[Any]"), ScalaType.int)
    )
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TTuple(Seq(TInt, TString))), SetSizeMono)))

    val key1 = Seq(
      BoolTrue,
      TupleLit(Seq(IntNum(1), StringLit("JGU"))),
      SetLit.from(TupleLit(Seq(IntNum(1), StringLit("Info"))))
    )

    val key2 = Seq(
      BoolTrue,
      TupleLit(Seq(IntNum(1), StringLit("JGU"))),
      SetLit.from(TupleLit(Seq(IntNum(1), StringLit("Info"))))
    )

    val tp1 = makeTp(
      TupleLit.apply,
      key1 :+ Cast(StringLit("PL"), ScalaType.any):_*
    )

    val tp2 = makeTp(
      TupleLit.apply,
      key1 :+ Cast(StringLit("Algorithm"), ScalaType.any): _*
    )


    val mainRelation = Relation("main", Seq(Param("size", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), tp1),
      WriteMono(Var("mono"), tp2),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("size"), Cast(nmapLookUp(Var("map"), key1:_*), TInt))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(2)(res.entries.head)


  test("Map mono basic test 23: 3-level nested map mono with recursive aggregation"):
    val mapMono = MapMonoDefinition(TInt, MapMonoDefinition(TDouble, MapMonoDefinition(TString, SumInt)))

    val mainRelation = Relation("main", Seq(Param("num", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      Eq(Var("node"), StringLit("A")),
      Call("collNode", Seq(Var("mono").arg, Var("node").arg)),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("num"), nmapLookUp(Var("map"), IntNum(1), DoubleNum(3), StringLit("A")))
    )))).addHint(MainHint)


    val collNode = Relation("collNode",
      Seq(
        Param("mono", TDemand(TMono(makeTp(TTuple.apply, TInt, TDouble, TString, TInt), TMap(TInt, TMap(TDouble, TMap(TString, TInt))), Seq()))),
        Param("t", TDemand(TString))
      ), Seq(Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t").arg)),
        WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(3), Var("t"), IntNum(1)))
      )),
        Body(Seq(
          ExtensionalCall("btree", Seq(Var("t").arg, Var("l").arg, Var("r").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("l").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("r").arg)),
          Eq(Var("map"), ReadMono(Var("mono"))),
          Eq(Var("lh"), nmapLookUp(Var("map"), IntNum(1), DoubleNum(3), Var("l"))),
          Eq(Var("rh"), nmapLookUp(Var("map"), IntNum(1), DoubleNum(3), Var("r"))),
          Eq(Var("height"), Add(Max(Var("lh"), Var("rh")), IntNum(1))),
          WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(3), Var("t"), Var("height")))
        ))
      ))
    val extLeaf = ExtensionalRelation(
      "leaf", Seq(Param("t", TString))
    )
    val extBTree = ExtensionalRelation(
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
    val engine = compile(DRedReteBackendFactory.INSTANCE, mainRelation, collNode, extLeaf, extBTree)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(3)(res.entries.head)

  test("Map mono basic test 24: 3-level nested multi-map mono with primitive key types"):
    val mapMono = MapMonoDefinition(TInt, MapMonoDefinition(TDouble, MapMonoDefinition(TString, SetMonoDefinition(TInt))))
    val mainRelation = Relation("main", Seq(Param("year", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(2.5), StringLit("JGU"), IntNum(2024))),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, IntNum(1), DoubleNum(2.5), StringLit("JGU"), IntNum(1946))),
      SetMember(Var("year"), nmapLookUp(ReadMono(Var("mono")), IntNum(1), DoubleNum(2.5), StringLit("JGU"))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set(2024, 1946))(res.entries.toSet)


  test("Map mono basic test 25: 2-level multi-map mono with non-primitive key types"):
    val mapMono = MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TInt, SetMonoDefinition(TBoolean)))
    val mainRelation = Relation("main", Seq(Param("res", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      WriteMono(Var("mono"), makeTp(TupleLit.apply, TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1), BoolFalse)),
      SetMember(Var("res"), nmapLookUp(ReadMono(Var("mono")), TupleLit(Seq(IntNum(1), StringLit("1"))), IntNum(1)))
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set(0))(res.entries.toSet)


  test("Map mono basic test 26: 3-level nested map mono with non-primitive key types"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TInt), SetMonoDefinition(TBoolean))))
    val mainRelation = Relation("main", Seq(Param("res1", TBoolean), Param("res2", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolFalse)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)), BoolTrue)),
      SetMember(Var("res1"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)))),
      SetMember(Var("res2"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(IntNum(1)))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set((0, 1), (0, 0)))(res.entries.toSet)

  test("Map mono basic test 27: 3-level nested multi-map mono with non-primitive key types"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TTuple(Seq(TInt, TString))), SetMonoDefinition(TBoolean))))
    val mainRelation = Relation("main", Seq(Param("res1", TBoolean), Param("res2", TBoolean)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolFalse)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolTrue)),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), BoolFalse)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("2")))), BoolTrue)),
      SetMember(Var("res1"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
      SetMember(Var("res2"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set((1, 0), (0, 0)))(res.entries.toSet)


  test("Map mono basic test 28: 3-level nested multi-map mono with tuple types in set mono"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TTuple(Seq(TInt, TString))), SetMonoDefinition(TTuple(Seq(TInt, TString))))))
    val mainRelation = Relation("main", Seq(Param("res1", TTuple(Seq(TInt, TString))), Param("res2", TTuple(Seq(TInt, TString)))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), TupleLit(Seq(IntNum(1), StringLit("-1"))))),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), TupleLit(Seq(IntNum(2), StringLit("-2"))))),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), TupleLit(Seq(IntNum(3), StringLit("-3"))))),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("2")))), TupleLit(Seq(IntNum(4), StringLit("-4"))))),
      SetMember(Var("res1"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
      SetMember(Var("res2"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set((2, "-2", 3, "-3"), (1, "-1", 3, "-3")))(res.entries.toSet)


  test("Map mono basic test 29: 3-level nested multi-map mono with set types in set mono"):
    val mapMono = MapMonoDefinition(TBoolean, MapMonoDefinition(TTuple(Seq(TInt, TString)), MapMonoDefinition(TSet(TTuple(Seq(TInt, TString))), SetMonoDefinition(TSet(TTuple(Seq(TInt, TString)))))))
    val mainRelation = Relation("main", Seq(Param("res1", TTuple(Seq(TInt, TString))), Param("res2", TTuple(Seq(TInt, TString)))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono1"), NewMono(mapMono)),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("-1")))))),
      WriteMono(Var("mono1"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), SetLit.from(TupleLit(Seq(IntNum(2), StringLit("-2")))))),
      Eq(Var("mono2"), NewMono(mapMono)),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))), SetLit.from(TupleLit(Seq(IntNum(3), StringLit("-3")))))),
      WriteMono(Var("mono2"), makeTp(TupleLit.apply, BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("2")))), SetLit.from(TupleLit(Seq(IntNum(4), StringLit("-4")))))),
      SetMember(Var("res1$tmp"), nmapLookUp(ReadMono(Var("mono1")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
      SetMember(Var("res2$tmp"), nmapLookUp(ReadMono(Var("mono2")), BoolTrue, TupleLit(Seq(IntNum(1), StringLit("1"))), SetLit.from(TupleLit(Seq(IntNum(1), StringLit("1")))))),
      SetMember(Var("res1"), Var("res1$tmp")),
      SetMember(Var("res2"), Var("res2$tmp")),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assertResult(Set((2, "-2", 3, "-3"), (1, "-1", 3, "-3")))(res.entries.toSet)


  test("Map mono basic test 30: multi-map mono with recursive aggregation"):
    val mapMono = MapMonoDefinition(TString, SetMonoDefinition(TString))

    val mainRelation = Relation("main", Seq(Param("Achild", TString), Param("Bchild", TString)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      Call("collNode", Seq(Var("mono").arg, StringLit("A").arg)),
      Call("collNode", Seq(Var("mono").arg, StringLit("F").arg)),
      Eq(Var("map"), ReadMono(Var("mono"))),
      SetMember(Var("Achild"), nmapLookUp(Var("map"), StringLit("A"))),
      SetMember(Var("Bchild"), nmapLookUp(Var("map"), StringLit("F"))),
    )))).addHint(MainHint)


    val collNode = Relation("collNode",
      Seq(
        Param("mono", TDemand(TMono(makeTp(TTuple.apply, TString, TString), TMap(TString, TSet(TString)), Seq()))),
        Param("t", TDemand(TString))
      ), Seq(Body(Seq(
        ExtensionalCall("leaf", Seq(Var("t").arg)),
        WriteMono(Var("mono"), makeTp(TupleLit.apply, Var("t"), Var("t")))
      )),
        Body(Seq(
          ExtensionalCall("btree", Seq(Var("t").arg, Var("l").arg, Var("r").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("l").arg)),
          Call("collNode", Seq(Var("mono").arg, Var("r").arg)),
          Eq(Var("map"), ReadMono(Var("mono"))),
          Eq(Var("lh"), nmapLookUp(Var("map"), Var("l"))),
          Eq(Var("rh"), nmapLookUp(Var("map"), Var("r"))),
          SetMember(Var("node"), SetUnion(SetUnion(Var("lh"), Var("rh")), SetLit.from(Var("t")))),
          WriteMono(Var("mono"), makeTp(TupleLit.apply, Var("t"), Var("node")))
        ))
      ))
    val extLeaf = ExtensionalRelation(
      "leaf", Seq(Param("t", TString))
    )
    val extBTree = ExtensionalRelation(
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
    val engine = compile(DRedReteBackendFactory.INSTANCE, mainRelation, collNode, extLeaf, extBTree)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)

  test("Map mono basic test 31: 2-level nested map mono "):
    val mapMono = MapMonoDefinition(TInt, MapMonoDefinition(TInt, SumInt))
    val mainRelation = Relation("main", Seq(Param("x", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
//      Eq(Var("x"), nmapLookUp(ReadMono(Var("mono")), IntNum(1))),
      Eq(Var("x"), nmapLookUp(ReadMono(Var("mono")), IntNum(1), IntNum(1))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))


  test("Map mono basic test 32: 2-level nested map mono "):
    val mapMono = MapMonoDefinition(TInt, SumInt)
    val mainRelation = Relation("main", Seq(Param("x", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(mapMono)),
      Eq(Var("x"), MapLookUp(ReadMono(Var("mono")), IntNum(1))),
    )))).addHint(MainHint)

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
