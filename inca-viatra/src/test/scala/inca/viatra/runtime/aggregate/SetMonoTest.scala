package inca.viatra.runtime.aggregate

import inca.ir
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TermArg, Var, WildcardArg, string2name}
import inca.ir.util.SourceLocation
import inca.foreign.scala.ir.{arithmetic, primitive, set}
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet}
import inca.ir.extension.tuple.{TTuple, Lowering => tupleLowering}
import inca.ir.extension.{block, demand, arithmetic as incaArithmetic, set as incaSet}
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike


case class CompiledSetModule(mod: Module) extends CompiledModule:
  override def compilerOptions: CompilerOptions = CompilerOptions.default

  override def name: Name = mod.name

  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation

  override def ir: Module = mod

  private class ScalaSetTypeChecker extends IRTypechecker with primitive.Typechecker

  override def typechecker: BaseIRTypechecker = new ScalaSetTypeChecker

  // As we introduced foreign term during set lowering, so we cannot
  override def optimize(p: Seq[Module]): Seq[Module] = p

  setPipeline(List(
    () => new arithmetic.ScalaLowering {},
    () => new set.ScalaLowering {},
    () => new block.Lowering {},
    () => new tupleLowering {}
  ))


class SetMonoTest extends AnyFunSuiteLike:
  private val langs: Language = BaseIR.language + incaSet.IR + incaArithmetic.IR + block.IR

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod


  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledSetModule(mod)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

  test("Lower set literal"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s"), SetLit(Seq(IntNum(1), IntNum(2)))),
      )))
    )
    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(1, 2))(res.entries.head)

  test("Lower set membership"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt)), Param("i", TInt)),
      Seq(Body(Seq(
        Eq(Var("s"), SetLit(Seq(IntNum(1), IntNum(2)))),
        Eq(Var("i"), IntNum(2)),
        SetMember(Var("i"), Var("s"))
      )))
    )
    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult((Set(1, 2),2))(res.entries.head)


  test("Lower set literal 2"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Call("range", Seq(TermArg(Var("i")), TermArg(Var("j")))),
        Eq(Var("s"), SetLit.from(Var("i"), Var("j")))
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt), Param("j", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(-3)))),
        Body(Seq(Eq(Var("i"), IntNum(0)), Eq(Var("j"), IntNum(100)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(Set(1), Set(2, -3), Set(0, 100)))(res.entries.toSet)


  test("Lower set comprehension 1"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s"), SetComprehension(Var("i"), Seq(Call("range", Seq(TermArg(Var("i")), TermArg(Var("j"))))))),
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt), Param("j", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(-3)))),
        Body(Seq(Eq(Var("i"), IntNum(0)), Eq(Var("j"), IntNum(100)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 2, 0))(res.entries.head)


  test("Lower set comprehension 2"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s"), SetComprehension(Var("i"), Seq(Call("range", Seq(TermArg(Var("i")), WildcardArg()))))),
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt), Param("j", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(-3)))),
        Body(Seq(Eq(Var("i"), IntNum(0)), Eq(Var("j"), IntNum(100)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 2, 0))(res.entries.head)


  test("Lower set from 1"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TTuple.make(Seq(TInt, TInt))))),
      Seq(Body(Seq(
        Eq(Var("s"), SetFrom("range")),
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt), Param("j", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(-3)))),
        Body(Seq(Eq(Var("i"), IntNum(0)), Eq(Var("j"), IntNum(100)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set((1, 1), (2, -3), (0, 100)))(res.entries.head)

  test("Lower set from 2"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s"), SetFrom("range")),
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)))),
        Body(Seq(Eq(Var("i"), IntNum(0)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(0, 2, 1))(res.entries.head)


  test("Lower set union"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(IntNum(1), IntNum(2)))),
        Eq(Var("s2"), SetLit(Seq(IntNum(3), IntNum(2)))),
        Eq(Var("s"), SetUnion(Var("s1"), Var("s2")))
      )))
    )
    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(1, 2, 3))(res.entries.head)

  test("Lower set intersection"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(IntNum(1), IntNum(2)))),
        Eq(Var("s2"), SetLit(Seq(IntNum(3), IntNum(2)))),
        Eq(Var("s"), SetIntersection(Var("s1"), Var("s2")))
      )))
    )
    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(2))(res.entries.head)


