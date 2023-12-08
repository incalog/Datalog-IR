package inca.ir.extension.edbdata

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.demand.TDemand
import inca.ir.typing.IRTypechecker
import inca.ir.{Body, Eq, Language, Module, Param, Relation, TNothing, Var, string2name, ModuleEntry}
import org.scalatest.funsuite.AnyFunSuiteLike

class EdbDataTest extends AnyFunSuiteLike:
  def module(relations: ModuleEntry*): Module =
    val typechecker = new IRTypechecker {}
    val mod = Module("M", Language(IR), relations)
    try typechecker.checkProgram(Seq(mod))
    finally println(mod)
    mod

  test("Peano schema"){
    module(
      EdbNodeDefinition("Nat"),
      EdbNodeDefinition("Zero", "Nat"),
      EdbNodeDefinition("Succ", "Nat"),
      EdbFieldDefinition("Succ", "pred", TEdbNode("Nat"))
    )
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

