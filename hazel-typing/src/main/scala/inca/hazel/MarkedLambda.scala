package inca.hazel

import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.edbdata
import inca.ir.extension.edbdata.*
import inca.ir.extension.map.*
import inca.ir.extension.not.*
import inca.ir.extension.string.*

import scala.collection.mutable.ListBuffer

class MarkedLambda:
  val contents: ListBuffer[ModuleEntry] = ListBuffer.empty

  private val Type = TData("Type")
  /** Type 𝜏 ::= ?|num|bool|𝜏→𝜏|𝜏×𝜏 */
  contents ++= Seq(
    DataDefinition(Type.ref.name),
    CaseDefinition("Unknown", Seq(), Type),
    CaseDefinition("Num", Seq(), Type),
    CaseDefinition("Bool", Seq(), Type),
    CaseDefinition("Fun", Seq(Type, Type), Type),
    CaseDefinition("Prod", Seq(Type, Type), Type)
  )

  private val Exp = TEdbNode("Exp")
  contents ++= Seq(
    EdbNodeDefinition("Exp"),
    EdbNodeDefinition("ENum", "Exp"),
    EdbFieldDefinition("ENum", "num", TEdbValue(TInt)),
    EdbNodeDefinition("EVar", "Exp"),
    EdbFieldDefinition("EVar", "name", TEdbValue(TString)),
    EdbNodeDefinition("EAdd", "Exp"),
    EdbFieldDefinition("EAdd", "lhs", Exp),
    EdbFieldDefinition("EAdd", "rhs", Exp),
    // TODO ...
  )

  private val Ctx = TMap(TEdbValue(TString), Type)

  // use defs instead of val here, so that each Datalog node is distinct
  private def ctx = Var("ctx")
  private def e = Var("e")
  private def e(i: Int) = Var(s"e$i")
  private def mark = Var("mark")
  private def mark(i: Int) = Var(s"mark$i")
  private def ty = Var("ty")
  private def ty(i: Int) = Var(s"ty$i")
  private def x = Var("x")

  val markSyn = "markSyn"
  contents += Relation(markSyn,
    Seq(
      Param(ctx.name, TDemand(Ctx)),
      Param(e.name, TDemand(Exp)),
      Param(mark.name, TInt), // TODO
      Param(ty.name, Type)
    ),
    Seq(
      Body( // MKSNum
        EdbDeconstruct(e, "ENum") ++ Seq(
        Eq(mark, IntNum(-1)),  // TODO
        Eq(ty, Construct("Num", Seq()))
      )),
      Body( // MKSPlus
        EdbDeconstruct(e, "EAdd", "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
        Call(markSyn, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
        Deconstruct(ty(1), "Num", Seq()),
        Call(markSyn, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(2).arg)),
        Deconstruct(ty(2), "Num", Seq()),
        Eq(mark, IntNum(-1)), // TODO
        Eq(ty, Construct("Num", Seq()))
      )),
      Body( // MKSVar
        EdbDeconstruct(e, "EVar", "name" ->x) ++ Seq(
        Eq(mark, IntNum(-1)), // TODO
        Eq(ty, MapLookUp(ctx, x))
      )),
      Body( // MKSFree
        EdbDeconstruct(e, "EVar", "name" ->x) ++ Seq(
        Eq(mark, IntNum(-1)), // TODO
        Not(MapContains(ctx, x)),
        Eq(ty, Construct("Unknown", Seq()))
      ))
    )
  )

  def module: Module = Module(
    "Hazel", new Language(Set(BaseIR, arithmetic.IR, data.IR, demand.IR, edbdata.IR, map.IR, not.IR, string.IR)),
    contents.toList
  )

object MarkedLambda extends App:
  val module = new MarkedLambda().module
  val compiled = new CompiledHazelModule(module)
  println(module)
  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nChecked:")
  try compiled.checked
  finally println(module)
