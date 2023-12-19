package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.Module
import inca.frontend.oodl.typechecker.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.{BaseIR, CompiledModule, Name, Module as IRModule}
import inca.ir.extension.{aggregateset, block, bool, datamatch, demand, disjunction, impure, mono, not, set, tuple}
import inca.ir.visitors.BaseIRVisitor

import inca.util.Implicits._

case class CompiledOODLModule(fun: Module) extends CompiledModule:

  override def name: Name = fun.name

  override def sourceLocation: SourceLocation = fun.name

  lazy val typed: Module = {
    val typer: Typechecker = new Typechecker
    typer.typecheck(fun)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    fun
  }

  lazy val ssa: Module = {
    val compiler = new SSA
    val module = compiler.compileModule(typed)

    println("SSA: ")
    println(module)

    val typer: Typechecker = new Typechecker
    typer.typecheck(module)
    messages ++= typer.getErrors
    messages ++= typer.getWarnings
    stopIfNeeded()
    module
  }

  lazy val ir: IRModule = {
    val compiler = new GenerateIR
    val module = compiler.compileModule(ssa)
    module
  }

object CompiledOODLModule:
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new mono.Lowering {},
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new not.Lowering {},
    () => new disjunction.Lowering {},
    () => new impure.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
  ) // arith + string + data


//  {
//    param$9 == "NameAnalysis"
//    Impure(currentMutation$0 => Var$$target(v, {
//      {
//        ? OID( {
//          Prog$$defs({
//            NameAnalysis$$prog(this, prog$0); prog$0
//          }: ID, defs$2); defs$2
//        }: ID, C$22, _)
//      }
//      alt
//      {
//        ? SID$Def$Def( {
//          Prog$$defs({
//            NameAnalysis$$prog(this, prog$0); prog$0
//          }: ID, defs$2); defs$2
//        }: ID, C$22, _, _)
//      }
//      , dispatch$findByName$String$Def(C$22, D$22)
//      , findByName$String$Def(D$22, {
//        Prog$$defs({
//          NameAnalysis$$prog(this, prog$1); prog$1
//        }: ID, defs$3); defs$3
//      }: ID, {
//        Var$$name(v, name$1); name$1
//      }: TString, return$42);
//      return$42
//    }: ID, currentMutation$0), currentMutation$0 + 1)
//      return$41 ==(): ()
//      return$40 ==
//    return$41: ()
//  }
//
//
//
//
//  {
//    param$9 == "NameAnalysis"
//    Impure(currentMutation$0:
//    <TInt>=>
//      {NameAnalysis$$prog(this, prog$0)
//    , Prog$$defs(prog$0: ID, defs$2)
//    , ? OID(defs$2: ID, C$22, _)}
//    alt
//    {NameAnalysis$$prog(this, prog$0)
//    , Prog$$defs(prog$0: ID, defs$2)
//    , ? SID$Def$Def(defs$2: ID, C$22, _, _)}
//    , dispatch$findByName$String$Def(C$22, D$22), NameAnalysis$$prog(this, prog$1), Prog$$defs(prog$1: ID, defs$3), Var$$name(v, name$1), findByName$String$Def(D$22, defs$3: ID, name$1: TString, return$42), Var$$target(v, return$42: ID, currentMutation$0), currentMutation$0 + 1)
//    return$41 == (): ()
//    return$40 == return$41: ()
//    }
//
//
//    {param$9 == "NameAnalysis"
//    Impure(currentMutation$0:
//    <TInt>=>
//      {NameAnalysis$$prog(this, prog$0)
//    , Prog$$defs(prog$0: ID, defs$2)
//    , ? OID(defs$2: ID, C$22, _)}
//    alt
//    {NameAnalysis$$prog(this, prog$0)
//    , Prog$$defs(prog$0: ID, defs$2)
//    , ? SID$Def$Def(defs$2: ID, C$22, _, _)}
//    , dispatch$findByName$String$Def(C$22, D$22), NameAnalysis$$prog(this, prog$1), Prog$$defs(prog$1: ID, defs$3), Var$$name(v, name$1), findByName$String$Def(D$22, defs$3: ID, name$1: TString, return$42), Var$$target(v, return$42: ID, currentMutation$0), currentMutation$0 + 1)
//    return$41 == (): ()
//    return$40 == return$41: ()
//    }