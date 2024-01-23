package inca.viatra.edbdata

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.edbdata.*
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.typing.IRTypechecker
import inca.ir.util.SourceLocation
import inca.ir.{Body, CompiledModule, Eq, Language, Module, ModuleEntry, Name, Param, Relation, TNothing, Var, string2name}
import inca.util.compileroptions.CompilerOptions
import inca.viatra.Executor
import org.scalatest.funsuite.AnyFunSuiteLike
import truechange.EditScript

class EdbDataTest extends AnyFunSuiteLike:
  val exec = new Executor()

  class EdbCompiledModule(val ir: Module) extends CompiledModule:
    override def name: Name = ir.name
    override def compilerOptions: CompilerOptions = CompilerOptions.default
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
    lazy val engine: exec.Engine = exec.instantiate(this)

  def module(relations: ModuleEntry*): EdbCompiledModule =
    val typechecker = new IRTypechecker {}
    val mod = Module("M", Language(IR), relations)
    try typechecker.checkProgram(Seq(mod))
    finally println(mod)
    new EdbCompiledModule(mod)

  test("Peano schema"){
    val mod = module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat"))
    )
    mod.engine.feed.processEditScript(EditScript(Seq(

    )))
  }

  test("Type enumerate") {
    module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("nats", Seq(Param("n", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Nat")))
        ))
      )),
      Relation("succs", Seq(Param("n", TEdbNode("Succ"))), Seq(
        Body(Seq(
          Eq(Var("n"), LookupEdbType(TEdbNode("Succ")))
        ))
      ))
    )
  }

  test("Type field enumerate") {
    module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("preds", Seq(Param("s", TEdbNode("Succ")), Param("p", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )
  }

  test("Type field lookup") {
    module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("preds", Seq(Param("s", TDemand(TEdbNode("Succ"))), Param("p", TEdbNode("Nat"))), Seq(
        Body(Seq(
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )
  }

  test("Type field inverse lookup") {
    module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat")),

      Relation("preds", Seq(Param("s", TEdbNode("Succ")), Param("p", TDemand(TEdbNode("Nat")))), Seq(
        Body(Seq(
          Eq(Var("p"), LookupEdbField(Var("s"), "pred"))
        ))
      ))
    )
  }

  test("Exp schema") {
    module(
      EdbNodeDefinition("Exp"),
      EdbNodeDefinition("Var", "Exp"),
      EdbFieldDefinition("Var", "name", TEdbValue(TString)),
      EdbNodeDefinition("Num", "Exp"),
      EdbFieldDefinition("Num", "value", TEdbValue(TInt)),
      EdbNodeDefinition("Add", "Exp"),
      EdbFieldDefinition("Add", "lhs", TEdbNode("Exp")),
      EdbFieldDefinition("Add", "rhs", TEdbNode("Exp"))
    )
  }

