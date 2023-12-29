package inca.viatra.runtime.aggregate

import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker, TypeErrorException}
import inca.ir.{BaseIR, Body, Call, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TermArg, Var, WildcardArg, string2name}
import inca.ir.util.SourceLocation
import inca.foreign.scala.ir.{arithmetic, bool, primitive, set, tuple, string, data}
import inca.ir.execution.{ExecutorEngine, IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.mono.{NaiveSetMonoDefinition, NewMono, ReadMono, WriteMono}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet}
import inca.ir.extension.tuple.{TTuple, TupleLit, IR as tupleIR, Lowering as tupleLowering}
import inca.ir.extension.{block, demand, arithmetic as incaArithmetic, set as incaSet}
import inca.ir.extension.arithmetic.Add
import inca.ir.extension.bool.{BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.string.{StringLit, TString, IR => stringIR}
import inca.ir.visitors.BaseIRVisitor
import inca.util.compileroptions.CompilerOptions


case class CompiledTupleModule(mod: Module) extends CompiledModule:
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
    () => new scalaLowering {},
    () => new demandLowering {}, // TODO: let lowering in inca-ir can lower arguments of foreign terms in an implicit way
    () => new blockLowering {}
  ))


//  setPipeline(List(
//    () => new arithmetic.ScalaLowering {},
//    () => new bool.ScalaLowering {},
//    () => new string.ScalaLowering {},
//    () => new set.ScalaLowering {},
//    () => new tuple.ScalaLowering {},
//    () => new demandLowering {}, // TODO: let lowering in inca-ir can lower arguments of foreign terms in an implicit way
//    () => new blockLowering {}
//  ))


class ScalaTupleLoweringTest extends AnyFunSuiteLike:
  private val langs: Language = BaseIR.language + incaSet.IR + incaArithmetic.IR + block.IR + demand.IR + tupleIR + boolIR + stringIR

  private def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod

  private def compile(relations: ModuleEntry*): ExecutorEngine =
    val mod = Module("M", langs, relations)
    val compiledMod = CompiledTupleModule(mod)
    val exec: IRExecutor = inca.viatra.Executor
    exec.instantiate(compiledMod)

  test("Lower tuple literal: 1"):
    val relation = Relation("main", Seq(Param("t", TTuple(Seq(TInt, TBoolean)))), Seq(Body(Seq(
      Eq(Var("t"), TupleLit(Seq(IntNum(1), BoolTrue)))
    ))))

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult((1, true))(res.entries.head)

  test("Lower tuple literal: 2"):
    val relation = Relation("main", Seq(Param("t", TTuple(Seq(TInt, TTuple(Seq(TBoolean, TString)))))), Seq(Body(Seq(
      Eq(Var("t"), TupleLit(Seq(IntNum(1), TupleLit(Seq(BoolTrue, StringLit("1"))))))
    ))))

    val engine = compile(relation)
    engine.readAll().foreach(res => println(res.asTable))
    val res = engine.read(UnitRelation("main"))
    assert(res.nonEmpty)
    assertResult((1, (true, "1")))(res.entries.head)

  test("Lower tuple literal: 3"):
    val relation = Relation("main", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
      Eq(TupleLit(Seq(Var("a"), Var("b"))), TupleLit(Seq(IntNum(1), IntNum(2))))
    ))))
    // Problem: if tuple is compiled into Scala terms, arguments "c" and "d" cannot be unbound variables
    assertThrows[TypeErrorException] {
      val engine = compile(relation)
    }


