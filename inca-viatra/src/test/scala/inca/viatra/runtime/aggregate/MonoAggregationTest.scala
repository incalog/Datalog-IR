package inca.viatra.runtime.aggregate

import inca.ir.execution.{IRExecutor, UnitRelation}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.monotypes.ArithmeticMono.CountMono
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.{BaseIR, Body, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, Var, string2name}
import inca.ir.extension.{aggregate, arithmetic, block, data, demand, impure, monotypes, string}
import inca.ir.extension.monotypes.{CompiledMonoModule, MkMono, ResultMono}
import org.scalatest.funsuite.AnyFunSuiteLike


class MonoAggregationTest extends AnyFunSuiteLike {
  private val langs : Language = BaseIR.language +
    arithmetic.IR + 
    demand.IR + 
    data.IR + 
    aggregate.IR +
    monotypes.IR +
    impure.IR +
    block.IR +
    string.IR
  
  def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod

  private lazy val relation1: Relation = Relation(
    "main",
    Seq(
      Param("t", TString),
//      Param("b", TInt)
    ),
    Seq(Body(Seq(
//      ExtensionalCall(Name("main$input"), Seq(Var(Name("MonoImpurity")))),
      Eq(Var("m"), MkMono(CountMono, Seq(), Seq(TString))),
      Eq(Var("t"), StringLit("A")),
      Eq(Var("b"), ResultMono(Var("m")))
  ))))

  private lazy val relation2: Relation = Relation(
    "main",
    Seq(Param("t", TInt)),
    Seq(Body(Seq(Eq(Var("t"), IntNum(1)))))
  )

  private lazy val relation3: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )


  test("Test case 1"){
    val mod = module(relation2)
    val compiledMod = CompiledMonoModule(mod)
    compiledMod.setPipeline(CompiledMonoModule.pipeline)
    val exec: IRExecutor = inca.viatra.Executor
    val engine = exec.instantiate(compiledMod)
    println(engine.readAll())
  }


  test("Test case 2") {
    val mod = module(relation1)
    println("Compile module")
    val compiledMod = CompiledMonoModule(mod)
    compiledMod.setPipeline(CompiledMonoModule.pipeline)
    println("After pipeline lowering" + compiledMod.lowered)
    val exec: IRExecutor = inca.viatra.Executor
    val engine = exec.instantiate(compiledMod)
    println(engine.readAll())
  }
}
