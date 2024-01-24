package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir as scalaExt
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaMonoDefinition, ScalaType}
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.ir.execution.{ExecutorEngine, IRExecutor, Relation1, Relation3, UnitRelation}
import inca.ir.extension.arithmetic.{Add, GT, IntNum, Max, Mul, Sub, TInt}
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.ConvertForeignIR
import inca.ir.extension.impure.{Impure, MainHint}
import inca.ir.{BaseIR, Body, Call, Cast, CompiledModule, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, TAny, Var, WildcardArg, string2name}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{ArithmeticMonoDefinition, DisjMonoDefinition, MapMonoDefinition, MonoImpurityKind, MonoTypes, NewMono, ReadMono, SetMonoDefinition, TMono, WriteMono}
import inca.ir.extension.set.{SetLit, SetMember, TSet, IR as setIR, Lowering as setLowering}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.{arithmetic, block, bool, data, demand, map, not, set, string, tuple}
import inca.ir.extension.{disjunction, impure, mono}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.aggregate.builtin.arithmetic.SumIntMono
import org.scalatest.funsuite.AnyFunSuiteLike


case class CompiledScalaMapMonoModule(mod: Module) extends CompiledModule:
  override def compilerOptions: CompilerOptions = CompilerOptions.default

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


  setPipeline(List(
    () => new mono.Lowering(optimizeMono = false) {},
    () => new MonoScalaLowering {},
    () => new ConversionElimination {},
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



class ScalaMapMonoTest extends AnyFunSuiteLike {

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

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod

  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledScalaMapMonoModule(mod)
    val exec: IRExecutor = inca.viatra.Executor()
    exec.instantiate(compiledMod)


  // Reason: we cannot enumerate keys of a map because keys are demanded inputs.
  test("Unsuccessful try of using map comprehension to transform a map to another map"):
    val mainRelation = Relation("main", Seq(Param("foo", TInt)), Seq(Body(Seq(
      Eq(Var("map1"), MapLit(Seq((IntNum(1), IntNum(2)), (IntNum(3), IntNum(4))))),
      Eq(Var("map2"), MapComprehension(Var("key"), Var("value"), Seq(
        MapContains(Var("map1"), Var("key")),
        Eq(Var("value"), MapLookUp(Var("map1"), Var("key")))
      ))),
      Eq(Var("foo"), MapLookUp(Var("map2"), IntNum(1)))
    ))))

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.isEmpty)


  test("Use MapFrom to generate a map from another map"):
    val mainRelation = Relation("main", Seq(Param("foo", TInt)), Seq(Body(Seq(
      Eq(Var("map1"), MapLit(Seq((IntNum(1), IntNum(2)), (IntNum(3), IntNum(4))))),
      Eq(Var("map2"), MapFun(Seq(Param("key", TInt)), MapLookUp(Var("map1"), Var("key")))),
      Eq(Var("foo"), MapLookUp(Var("map2"), IntNum(1)))
    ))))

    val engine = compile(mainRelation)
    //engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(2)(res.entries.head)


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
      constructorParamTypes = Seq(),
      typ = MonoTypes(ScalaType("Any"), ScalaType("Set[Any]"), ScalaType.int)
    )

    val mainRelation = Relation("main", Seq(Param("size", TInt)), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetSizeMono))),
      WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))),
        Cast(TupleLit(Seq(IntNum(1), StringLit("1"))), ScalaType.any)
      ))),
      WriteMono(Var("mono"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))),
        Cast(TupleLit(Seq(IntNum(-1), StringLit("-1"))), ScalaType.any)
      ))),
      Eq(Var("map"), ReadMono(Var("mono"))),
      Eq(Var("size"), ConvertForeignIR(
        MapLookUp(Var("map"), TupleLit(Seq(StringLit("-1"), IntNum(1)))),
        ScalaType.int,
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

    val engine = compile(mainRelationSucc, collNode, extLeaf, extBTree)
//    val engine = compile(mainRelationFail, collNode, extLeaf, extBTree)
    engine.insert(edbLeaf)
    engine.insert(edbBTree)
    //engine.readAll().foreach(res => println(res.asTable))



  test("Map momo basic test 16: value mono is another map mono"):
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


  // TODO: 1. MapMono with user-defined mono ✔
  //       2. MapMono with map mono ✔ (not successful)
  //       3. MapMono whose keys can be lowered ✔
  //       4. MapMono with recursive relations ✔ (not successful)
  //       5. Two Map Monos ✔
  //
}
