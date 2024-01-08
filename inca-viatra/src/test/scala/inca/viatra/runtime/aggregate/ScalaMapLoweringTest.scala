package inca.viatra.runtime.aggregate

import inca.foreign.scala.ir as scalaExt
import inca.foreign.scala.ir.primitive.ForeignScalaLowering
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, GT, IntNum, Mul, Sub, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, Var, WildcardArg, string2name}
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.set.{SetLit, TSet}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{TTuple, TupleLit}
import inca.ir.extension.{arithmetic, block, demand, set}
import inca.ir.extension.disjunction
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.HashMap


case class CompiledScalaMapModule(mod: Module) extends CompiledModule:
  override def compilerOptions: CompilerOptions = CompilerOptions.default

  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

  override def ir: Module = mod

  private class ScalaSetTypeChecker extends IRTypechecker with scalaExt.primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new ScalaSetTypeChecker

  // As we introduced foreign term during set lowering, so we cannot
  override def optimize(p: Seq[Module]): Seq[Module] = p

  private trait demandLowering extends demand.Lowering with scalaExt.primitive.Visitor

  private trait blockLowering extends block.Lowering with scalaExt.primitive.Visitor

  private trait disjunctionLowering extends disjunction.Lowering with scalaExt.primitive.Visitor


  setPipeline(List(
    () => new ForeignScalaLowering {},
    () => new blockLowering {},
    () => new disjunctionLowering {}, // disjunction lowering can only work after elimination of block
    () => new demandLowering {}, // TODO: let lowering in inca-ir can lower arguments of foreign terms in an implicit way
  ))


class ScalaMapLoweringTest extends AnyFunSuiteLike:
  private val langs: Language = BaseIR.language + set.IR + arithmetic.IR + block.IR + demand.IR + mapIR + disjunction.IR

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledScalaMapModule(mod)
    val exec: IRExecutor = new inca.viatra.Executor
    exec.instantiate(compiledMod)

  test("Lower map literal: 1"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TString, TInt))), Seq(Body(Seq(
      Eq(Var("map"), MapLit.from((StringLit("p"), IntNum(1)), (StringLit("q"), IntNum(2))))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map("p"->1, "q"->2))(res.entries.head)

  test("Lower map literal: 2"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TString, TSet(TInt)))), Seq(Body(Seq(
      Eq(Var("p"), StringLit("p")),
      Eq(Var("q"), StringLit("q")),
      Eq(Var("i"), SetLit.from(IntNum(1), IntNum(2))),
      Eq(Var("j"), SetLit.from(IntNum(2), IntNum(3))),
      Eq(Var("map"), MapLit.from((Var("p"), Var("i")), (Var("q"), Var("j"))))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map("p" -> Set(1, 2), "q" -> Set(2, 3)))(res.entries.head)


  test("Lower map literal: 3"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TString, TMap(TString, TSet(TInt))))), Seq(Body(Seq(
      Eq(Var("p"), StringLit("p")),
      Eq(Var("q"), StringLit("q")),
      Eq(Var("i"), SetLit.from(IntNum(1), IntNum(2))),
      Eq(Var("j"), SetLit.from(IntNum(2), IntNum(3))),
      Eq(Var("m1"), MapLit.from((Var("q"), Var("i")))),
      Eq(Var("m2"), MapLit.from((Var("p"), Var("j")))),
      Eq(Var("map"), MapLit.from((Var("p"), Var("m1")), (Var("q"), Var("m2"))))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map("p" -> Map("q" -> Set(1, 2)), "q" -> Map("p" -> Set(2, 3))))(res.entries.head)

  test("Lower map look up: 1"):
    val mainRelation = Relation("main", Seq(Param("i", TInt)), Seq(Body(Seq(
      Eq(Var("p"), StringLit("p")),
      Eq(Var("q"), StringLit("q")),
      Eq(Var("map"), MapLit.from((Var("p"), IntNum(1)), (Var("q"), IntNum(2)))),
      Eq(Var("i"), MapLookUp(Var("map"), Var("p")))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(1)(res.entries.head)

  test("Lower map look up: 2"):
    val mainRelation = Relation("main", Seq(Param("i", TInt)), Seq(Body(Seq(
      Eq(Var("p"), StringLit("p")),
      Eq(Var("q"), StringLit("q")),
      Eq(Var("map"), MapLit.from((Var("p"), IntNum(1)), (Var("q"), IntNum(2)))),
      Eq(Var("i"), MapLookUp(Var("map"), StringLit("r")))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.isEmpty)


  test("Lower map look up: 3"):
    val mainRelation = Relation("main", Seq(Param("s", TSet(TInt))), Seq(Body(Seq(
      Eq(Var("p"), StringLit("p")),
      Eq(Var("q"), StringLit("q")),
      Eq(Var("i"), SetLit.from(IntNum(1), IntNum(2))),
      Eq(Var("j"), SetLit.from(IntNum(2), IntNum(3))),
      Eq(Var("m1"), MapLit.from((Var("q"), Var("i")))),
      Eq(Var("m2"), MapLit.from((Var("p"), Var("j")))),
      Eq(Var("m3"), MapLit.from((Var("p"), Var("m1")), (Var("q"), Var("m2")))),
      Eq(Var("m4"), MapLookUp(Var("m3"), Var("p"))),
      Eq(Var("s"), MapLookUp(Var("m4"), Var("q")))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(1, 2))(res.entries.head)

  test("Lower map from: 1"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TInt, TInt))), Seq(Body(Seq(
      Call("succ", Seq(IntNum(2024).arg, WildcardArg())),
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("map"), MapFrom("succ"))
    ))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map(1 -> 2, 2 -> 3, 2024 -> 2025))(res.entries.head)

  test("Lower map from: 2"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TTuple(Seq(TInt, TInt)), TInt))), Seq(Body(Seq(
      Call("plus", Seq(IntNum(5).arg, WildcardArg(), IntNum(1).arg)),
      Call("plus", Seq(IntNum(1).arg, WildcardArg(), IntNum(5).arg)),
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("map"), MapFrom("plus")),
    ))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val plusRelation = Relation("plus", Seq(Param("i", TDemand(TInt)), Param("k", TInt), Param("j", TDemand(TInt))), Seq(Body(Seq(
      GT(Var("j"), IntNum(0)),
      Eq(Var("j$0"), Sub(Var("j"), IntNum(1))),
      Call("plus", Seq(Var("i").arg, Var("k$0").arg, Var("j$0").arg)),
      Eq(Var("k"), Add(IntNum(1), Var("k$0")))
    )), Body(Seq(
      Eq(Var("j"), IntNum(0)),
      Eq(Var("i"), Var("k"))
    ))))

    val engine = compile(mainRelation, succRelation, plusRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(HashMap((1,2) -> 3, (1,4) -> 5, (1,1) -> 2, (5,0) -> 5, (5,1) -> 6, (1,3) -> 4, (1,5) -> 6, (1,0) -> 1))(res.entries.head)


  test("Lower map look up: 4"):
    val mainRelation = Relation("main", Seq(Param("v", TInt)), Seq(Body(Seq(
      Call("succ", Seq(IntNum(2024).arg, WildcardArg())),
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("v"), MapLookUp(MapFrom("succ"), IntNum(1)))
    ))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(2)(res.entries.head)

  test("Lower map plus: 1"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TInt, TInt))), Seq(Body(Seq(
      Call("succ", Seq(IntNum(2024).arg, WildcardArg())),
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("map"), MapPlus(MapFrom("succ"), IntNum(3), IntNum(0)))
    ))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map(1 -> 2, 2 -> 3, 2024 -> 2025, 3 -> 0))(res.entries.head)


  test("Lower map plus: 2"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TTuple.make(Seq(TInt, TInt)), TInt))), Seq(Body(Seq(
      Call("plus", Seq(IntNum(1).arg, WildcardArg(), IntNum(3).arg)),
      Eq(Var("map"), MapPlus(MapFrom("plus"), TupleLit(Seq(IntNum(1), IntNum(2))), IntNum(2024)))
    ))))

    val plusRelation = Relation("plus", Seq(Param("i", TDemand(TInt)), Param("k", TInt), Param("j", TDemand(TInt))), Seq(Body(Seq(
      GT(Var("j"), IntNum(0)),
      Eq(Var("j$0"), Sub(Var("j"), IntNum(1))),
      Call("plus", Seq(Var("i").arg, Var("k$0").arg, Var("j$0").arg)),
      Eq(Var("k"), Add(IntNum(1), Var("k$0")))
    )), Body(Seq(
      Eq(Var("j"), IntNum(0)),
      Eq(Var("i"), Var("k"))
    ))))

    val engine = compile(mainRelation, plusRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map((1, 1) -> 2, (1, 2) -> 2024, (1, 0) -> 1, (1, 3) -> 4))(res.entries.head)

  test("Lower map union: 1"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TInt, TInt))), Seq(Body(Seq(
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Call("double", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("map"), MapUnion(MapFrom("succ"), MapFrom("double"))
    )))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val doubleRelation = Relation("double", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Mul(Var("i"), IntNum(2)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation, doubleRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Seq(Map(2->3), Map(2->4)))(res.entries)

  test("Lower map concatenation: 1"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TInt, TInt))), Seq(Body(Seq(
      Call("succ", Seq(IntNum(2).arg, WildcardArg())),
      Call("double", Seq(IntNum(2).arg, WildcardArg())),
      Eq(Var("map"), MapConcat(MapFrom("succ"), MapFrom("double"))
    )))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val doubleRelation = Relation("double", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Mul(Var("i"), IntNum(2)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation, doubleRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Seq(Map(2 -> 4)))(res.entries)

  test("Lower map contains"):
    val mainRelation = Relation("main", Seq(Param("k", TInt)), Seq(Body(Seq(
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(3).arg, WildcardArg())),
      MapContains(MapFrom("succ"), Var("k"))
    ))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))

    val engine = compile(mainRelation, succRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(1, 3))(res.entries.toSet)

  test("Lower map comprehension: 1"):
    val mainRelation = Relation("main", Seq(Param("map2", TMap(TInt, TInt))), Seq(Body(Seq(
      Eq(Var("map1"), MapLit.from((IntNum(1), IntNum(2)), (IntNum(3), IntNum(4)))),
      Eq(Var("map2"), MapComprehension(Var("k"), MapLookUp(Var("map1"), Var("k")), Seq(MapContains(Var("map1"), Var("k")))))
    ))))

    val engine = compile(mainRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map(1 -> 2, 3 -> 4))(res.entries.head)


  test("Lower map comprehension: 2"):
    val mainRelation = Relation("main", Seq(Param("map", TMap(TInt, TInt))), Seq(Body(Seq(
      Call("succ", Seq(IntNum(1).arg, WildcardArg())),
      Call("succ", Seq(IntNum(3).arg, WildcardArg())),
      Eq(Var("map"), MapComprehension(Var("k"), Add(MapLookUp(MapFrom("succ"), Var("k")), IntNum(1)), Seq(MapContains(MapFrom("succ"), Var("k"))))
    )))))

    val succRelation = Relation("succ", Seq(Param("i", TDemand(TInt)), Param("j", TInt)), Seq(Body(Seq(
      Eq(Add(Var("i"), IntNum(1)), Var("j"))
    ))))
    val engine = compile(mainRelation, succRelation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Map(1 -> 3, 3 -> 5))(res.entries.head)


