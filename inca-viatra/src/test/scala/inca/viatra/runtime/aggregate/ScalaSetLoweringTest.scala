package inca.viatra.runtime.aggregate

import inca.ir
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TermArg, Var, WildcardArg, string2name}
import inca.ir.util.SourceLocation
import inca.foreign.scala.ir.{arithmetic, primitive, set, tuple, bool, data, string}
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.mono.{NaiveSetMonoDefinition, NewMono, ReadMono, WriteMono}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet}
import inca.ir.extension.tuple.{TTuple, Lowering as tupleLowering}
import inca.ir.extension.{block, demand, arithmetic as incaArithmetic, set as incaSet}
import inca.ir.extension.arithmetic.Add
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

  private trait demandLowering extends demand.Lowering with primitive.Visitor
  private trait blockLowering extends block.Lowering with primitive.Visitor

  private trait scalaLowering extends primitive.ScalaLowering 
    with set.ScalaLowering 
    with tuple.ScalaLowering
    with bool.ScalaLowering
    with arithmetic.ScalaLowering
    with data.ScalaLowering
    with string.ScalaLowering
 
  
  setPipeline(List(
//    () => new arithmetic.ScalaLowering {},
//    () => new set.ScalaLowering {},
    () => new scalaLowering {},
    () => new demandLowering {}, // TODO: let lowering in inca-ir can lower arguments of foreign terms in an implicit way
    () => new blockLowering {}
  ))


class ScalaSetLoweringMonoTest extends AnyFunSuiteLike:
  private val langs: Language = BaseIR.language + incaSet.IR + incaArithmetic.IR + block.IR + demand.IR

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

  test("Lower set membership 1"):
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

  test("Lower set membership 2"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TInt)), Param("i", TInt)),
      Seq(Body(Seq(
        Eq(Var("s"), SetLit(Seq(IntNum(1), IntNum(2)))),
        SetMember(Var("i"), Var("s"))
      )))
    )
    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set((Set(1, 2), 2), (Set(1, 2), 1)))(res.entries.toSet)


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


  test("Lower set comprehension 3"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s"), SetComprehension(Var("j"), Seq(SetMember(Var("j"), SetLit(Seq(IntNum(1)))))))
      )))
    )

    val engine = compile(relation1)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1))(res.entries.head)


  test("Lower set comprehension 4"):
    val relation1 = Relation(
      "main",
      Seq(Param("s2", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetComprehension(Var("i"), Seq(Call("range", Seq(TermArg(Var("i")), TermArg(Var("j"))))))),
        Eq(Var("s2"), SetComprehension(Add(Var("k"), IntNum(1)), Seq(SetMember(Var("k"), Var("s1")))))
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
    assertResult(Set(1, 2, 3))(res.entries.head)


  test("Lower set comprehension 5"):
    val relation1 = Relation(
      "main",
      Seq(Param("s", TSet(TInt))),
      Seq(Body(Seq(
        Eq(Var("i"), IntNum(1)),
        Eq(Var("s"), SetComprehension(Var("j"), Seq(Call("range", Seq(TermArg(Var("i")), TermArg(Var("j"))))))),
      )))
    )

    val relation2 = Relation(
      "range",
      Seq(Param("i", TInt), Param("j", TInt)),
      Seq(
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(1)))),
        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(-3)))),
        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(100)))),
      )
    )

    val engine = compile(relation1, relation2)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.entries.nonEmpty)
    assertResult(Set(1, 100))(res.entries.head)


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


  test("Higher-order set: basic test"):
    val relation = Relation(
      "main",
      Seq(Param("s", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s"), SetLit(Seq(SetLit(Seq(IntNum(1))))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(1)))(res.entries.head)


  test("Higher-order set: comprehension: 1"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(IntNum(1)))),
        Eq(Var("s2"), SetComprehension(SetLit(Seq(Var("i"))), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(1)))(res.entries.head)

  test("Higher-order set: comprehension: 2"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(IntNum(1)))),
        Eq(Var("s2"), SetComprehension(SetUnion(SetLit(Seq(Var("i"))), SetLit(Seq(Add(Var("i"), IntNum(1))))), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(1, 2)))(res.entries.head)


  test("Higher-order set: comprehension 3"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TSet(TInt))))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(SetLit(Seq(IntNum(1)))))),
        Eq(Var("s2"), SetComprehension(SetLit(Seq(Var("i"))), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(Set(1))))(res.entries.head)

  test("Higher-order set: comprehension 4"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(SetLit.from(IntNum(1), IntNum(2)), SetLit.from(IntNum(2), IntNum(3))))),
        Eq(Var("s2"), SetComprehension(SetComprehension(Add(Var("j"), IntNum(1)), Seq(SetMember(Var("j"), Var("i")))), Seq(SetMember(Var("i"), Var("s1")))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(2, 3), Set(3, 4)))(res.entries.head)


  test("Higher-order set: union"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(SetLit.from(IntNum(1), IntNum(2)), SetLit.from(IntNum(2), IntNum(3))))),
        Eq(Var("s2"), SetUnion(Var("s1"), SetComprehension(SetComprehension(Add(Var("j"), IntNum(1)), Seq(SetMember(Var("j"), Var("i")))), Seq(SetMember(Var("i"), Var("s1"))))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(1, 2), Set(2, 3), Set(3, 4)))(res.entries.head)

  test("Higher-order set: intersection"):
    val relation = Relation(
      "main",
      Seq(Param("s2", TSet(TSet(TInt)))),
      Seq(Body(Seq(
        Eq(Var("s1"), SetLit(Seq(SetLit.from(IntNum(1), IntNum(2)), SetLit.from(IntNum(2), IntNum(3))))),
        Eq(Var("s2"), SetIntersection(Var("s1"), SetComprehension(SetComprehension(Add(Var("j"), IntNum(1)), Seq(SetMember(Var("j"), Var("i")))), Seq(SetMember(Var("i"), Var("s1"))))))
      )))
    )

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult(Set(Set(2, 3)))(res.entries.head)

  // VIATRA has some exception
//  test("Higher-order set: membership"):
//    val relation1 = Relation(
//      "main",
//      Seq(Param("k", TInt), Param("t", TInt), Param("s2", TSet(TSet(TInt)))),
//      Seq(Body(Seq(
//        Eq(Var("s1"), SetLit(Seq(SetLit.from(IntNum(1), IntNum(2)), SetLit.from(IntNum(2), IntNum(3))))),
//        Call("range", Seq(Var("k").arg, Var("t").arg)),
//        Eq(Var("s2"), SetComprehension(Var("i"), Seq(SetMember(Var("i"), Var("s1"))))),
////        SetMember(SetLit.from(IntNum(1), IntNum(2)), Var("s2"))
////        SetMember(SetLit.from(IntNum(1), IntNum(2)), SetComprehension(Var("i"), Seq(SetMember(Var("i"), Var("s1")))))
////        SetMember(SetLit.from(Var("k"), Var("t")), SetComprehension(Var("i"), Seq(SetMember(Var("i"), Var("s1")))))
////        SetMember(SetLit.from(Var("k"), Var("t")), SetComprehension(SetComprehension(Add(Var("j"), IntNum(1)), Seq(SetMember(Var("j"), Var("i")))), Seq(SetMember(Var("i"), Var("s1")))))
//      )))
//    )
//
//    val relation2 = Relation(
//      "range",
//      Seq(Param("i", TInt), Param("j", TInt)),
//      Seq(
//        Body(Seq(Eq(Var("i"), IntNum(1)), Eq(Var("j"), IntNum(2)))),
//        Body(Seq(Eq(Var("i"), IntNum(2)), Eq(Var("j"), IntNum(3)))),
//        Body(Seq(Eq(Var("i"), IntNum(0)), Eq(Var("j"), IntNum(100)))),
//      )
//    )



//    val engine = compile(relation1, relation2)
//    engine.readAll().foreach(res => println(res.asTable))
//    val res = engine.read(UnitRelation("main"))
//    assert(res.nonEmpty)
//    assertResult(Set(Set(2, 3)))(res.entries.head)

