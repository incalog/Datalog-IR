package inca.viatra.runtime.aggregate

import inca.ir.execution.{IRExecutor, Relation1}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.monotypes.ArithmeticMono.SumMono
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.{BaseIR, Body, Eq, ExtensionalCall, ExtensionalRelation, Language, Module, ModuleEntry, Name, Param, Relation, Var, string2name}
import inca.ir.extension.{aggregate, arithmetic, block, data, demand, impure, monotypes, string, bool}
import inca.ir.extension.monotypes.{AddMono, CompiledMonoModule, MkMono, ResultMono}
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
    string.IR +
    bool.IR
  
  def module(relations: ModuleEntry*): Module =
    val mod = Module("M", langs, relations)
    mod

  private lazy val relation1: Relation = Relation(
    "main",
    Seq(
      Param("b", TInt)
    ),
    Seq(Body(Seq(
      Eq(Var("m"), MkMono(SumMono, Seq(), Seq(TString))),
      Eq(Var("b"), ResultMono(Var("m")))
    )))).addHint(impure.Hints.Pure)


  private lazy val relation2: ExtensionalRelation = ExtensionalRelation(
    Name("main$input"), Seq(Param(Name("id"), TInt))
  )


  test("Test case 2") {
    val mod = module(relation1, relation2)
    println("Compile module")
    val compiledMod = CompiledMonoModule(mod)
    compiledMod.setPipeline(CompiledMonoModule.pipeline)
    println("After pipeline lowering" + compiledMod.lowered)
    val exec: IRExecutor = inca.viatra.Executor
    val engine = exec.instantiate(compiledMod)
    engine.insert(Relation1("main$input", Seq("id"), Seq(Seq(1))))
    println(engine.readAll())
  }
}
