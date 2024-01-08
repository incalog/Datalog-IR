package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir as scalaExt
import inca.foreign.scala.ir.primitive.{ForeignScalaLowering, ConversionElimination}
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, GT, IntNum, Mul, Sub, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.impure.{Impure, PureHint}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, Var, WildcardArg, string2name}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.mono.{ArithmeticMonoDefinition, MapMonoDefinition, MonoImpurityKind, NewMono, ReadMono, WriteMono}
import inca.ir.extension.set.{SetLit, TSet, IR as setIR, Lowering as setLowering}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TTuple, TupleLit, IR as tupleIR, Lowering as tupleLowering}
import inca.ir.extension.{arithmetic, block, bool, data, demand, map, set, string, tuple}
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

  setPipeline(List(
    () => new mono.Lowering(optimizeSetMono = false) {},
    () => new MonoScalaLowering {},
    () => new ConversionElimination {},
    () => new impure.Lowering {},
    () => new setLowering {},
    () => new bool.Lowering {},
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
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

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
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))


  test("Map Mono: basic test 2"):
    val mainRelation = Relation("main", Seq(Param("p", TMap(TInt, TInt))), Seq(Body(Seq(
      Eq(Var("counter"), IntNum(0)),
      Impure(Name("counter"), Seq(), Var("counter"), MonoImpurityKind),
      Eq(Var("mono"), NewMono(MapMonoDefinition(TInt, ArithmeticMonoDefinition.SumInt))),
      WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(1)))),
      WriteMono(Var("mono"), TupleLit(Seq(IntNum(1), IntNum(2)))),
      Eq(Var("p"), ReadMono(Var("mono")))
    )))).addHint(PureHint)

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))

}
