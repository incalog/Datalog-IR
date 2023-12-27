package inca.viatra.runtime.aggregate

import inca.ir
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TermArg, Var, WildcardArg, string2name}
import inca.ir.util.SourceLocation
import inca.foreign.scala.ir.{arithmetic, primitive, set}
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.mono.{MonoImpurityKind, NaiveSetMonoDefinition, NewMono, ReadMono, WriteMono}
import inca.ir.extension.mono
import inca.ir.extension.impure
import inca.ir.extension.impure.Impure
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet}
import inca.ir.extension.tuple.{TTuple, Lowering as tupleLowering}
import inca.ir.extension.{block, data, demand, impure, mono, string, arithmetic as incaArithmetic, set as incaSet}
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike
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
    () => new mono.Lowering {},
    () => new impure.Lowering {},
    () => new demandLowering {},
    () => new arithmetic.ScalaLowering {},
    () => new set.ScalaLowering {},
    () => new demandLowering {},
    () => new blockLowering {}
  ))


class SetMonoTest extends AnyFunSuiteLike {

  private val langs: Language = BaseIR.language +
    incaSet.IR +
    incaArithmetic.IR +
    block.IR +
    mono.IR +
    impure.IR +
    data.IR +
    string.IR

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledSetMonoModule(mod)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)



  test("Test naive set mono: basic test"):
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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1))(res.entries.head)


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

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(2, 3, 4))(res.entries.head)


}
