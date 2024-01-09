package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir as scalaExt
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering}
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, GT, IntNum, Mul, Sub, TInt}
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, Var, WildcardArg, string2name}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.mono.{ArithmeticMonoDefinition, DisjMonoDefinition, MapMonoDefinition, MonoImpurityKind, NewMono, ReadMono, SetMonoDefinition, WriteMono}
import inca.ir.extension.set.{SetLit, SetMember, TSet, IR as setIR, Lowering as setLowering}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TTuple, TupleLit, IR as tupleIR}
import inca.ir.extension.{arithmetic, block, bool, data, demand, map, set, string, tuple, not}
import inca.ir.extension.{disjunction, impure, mono}
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
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
    () => new mono.Lowering(optimizeSetMono = false) {},
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
    disjunction.IR

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
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.isEmpty)


  test("Use MapFrom to generate a map from another map"):
    val mainRelation = Relation("main", Seq(Param("foo", TInt)), Seq(Body(Seq(
      Eq(Var("map1"), MapLit(Seq((IntNum(1), IntNum(2)), (IntNum(3), IntNum(4))))),
      Eq(Var("map2"), MapFun(Seq(Param("key", TInt)), MapLookUp(Var("map1"), Var("key")))),
      Eq(Var("foo"), MapLookUp(Var("map2"), IntNum(1)))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(2)(res.entries.head)


  // mono = new MapMono[Int, Int](arithMonoDef)
  // Optimize map mono:
  // m@MapMono += (1, 2)
  // ==> m1@arithMono += 2@1
  // res = m@MapMono.get ==> relation doing aggregation on the arithmetic monos

  test("Map Mono: basic test 1"):
    val mainRelation = Relation("main", Seq(Param("p", TMap(TInt, TInt))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, ArithmeticMonoDefinition.SumInt))),
      Eq(Var("p"), ReadMono(Var("mono")))
    )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)


  test("Map Mono: basic test 2"):
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
      )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(3)(res.entries.head)

  test("Map Mono: basic test 3"):
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
      )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 2, 3))(res.entries.toSet)


  test("Map Mono: basic test 4"):
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
      )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((1, "1"), (2, "2")))(res.entries.toSet)


  test("Map Mono: basic test 5"):
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
      )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(1)(res.entries.head)


  test("Map Mono: basic test 6"):
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
      )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((1, "1"), (2, "2")))(res.entries.toSet)


//  test("Map Mono: basic test 7"):
//    val mainRelation = Relation("main",
//      Seq(Param("elem", TTuple(Seq(TInt, TString)))),
//      Seq(Body(Seq(
//        Eq(Var("counter"), IntNum(0)),
//        Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
//        Eq(Var("mono1"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
////        Eq(Var("mono2"), NewMono(MapMonoDefinition(TTuple(Seq(TString, TInt)), SetMonoDefinition(TTuple(Seq(TInt, TString)))))),
//        WriteMono(Var("mono1"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))), TupleLit(Seq(IntNum(1), StringLit("1")))))),
////        WriteMono(Var("mono1"), TupleLit(Seq(TupleLit(Seq(StringLit("-1"), IntNum(1))), TupleLit(Seq(IntNum(-1), StringLit("-1")))))),
////        WriteMono(Var("mono2"), TupleLit(Seq(TupleLit(Seq(StringLit("-2"), IntNum(2))), TupleLit(Seq(IntNum(2), StringLit("2")))))),
////        WriteMono(Var("mono2"), TupleLit(Seq(TupleLit(Seq(StringLit("-2"), IntNum(2))), TupleLit(Seq(IntNum(-2), StringLit("-2")))))),
//        Eq(Var("map"), ReadMono(Var("mono1"))),
//        SetMember(Var("elem"), MapLookUp(Var("map"), TupleLit(Seq(StringLit("-1"), IntNum(1)))))
//      )))).addHint(PureHint)
//
//    val engine = compile(mainRelation)
//    engine.readAll().foreach(res => println(res.asTable))
//    val res = engine.read(UnitRelation("main"))
//    assert(res.entries.nonEmpty)
//    assertResult(Set((2, "2"), (-2, "-2")))(res.entries.toSet)


  // TODO: 1. MapMono with user-defined mono
  //       2. MapMono with map mono
  //       3. MapMono whose keys can be lowered
  //       4. MapMono with ADT
  //       5. MapMono with MapUnion, ...
  //       6. MapMono with recursive relations
  //       7. Two Map Monos
  //
}
