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
  private val TUnknown = "Unknown";
  private val TNum = "Num"
  private val TBool = "Bool"
  private val TArrow = "Arrow"
  private val TProd = "Prod"
  contents ++= Seq(
    DataDefinition(Type.ref.name),
    CaseDefinition(TUnknown, Seq(), Type),
    CaseDefinition(TNum, Seq(), Type),
    CaseDefinition(TBool, Seq(), Type),
    CaseDefinition(TArrow, Seq(Type, Type), Type),
    CaseDefinition(TProd, Seq(Type, Type), Type)
  )

  private val ConstructTUnknown = Construct(TUnknown, Seq())
  private val ConstructTNum = Construct(TNum, Seq())
  private val ConstructTBool = Construct(TBool, Seq())
  private val ConstructTArrow = (ty1: Construct, ty2: Construct) =>
    Construct(TArrow, Seq(ty1, ty2))
  private val ConstructTProd = (ty1: Construct, ty2: Construct) =>
    Construct(TProd, Seq(ty1, ty2))

  private val Exp = TEdbNode("Exp")
  contents ++= Seq(
    EdbNodeDefinition(Exp.name),
    EdbNodeDefinition("EHole", Exp.name),
    EdbNodeDefinition("EVar", Exp.name),
    EdbFieldDefinition("EVar", "name", TEdbValue(TString)),
    EdbNodeDefinition("ELam", Exp.name),
    EdbFieldDefinition("ELam", "param", TEdbValue(TString)),
    EdbFieldDefinition("ELam", "ty", TEdbValue(Type)),
    EdbFieldDefinition("ELam", "body", Exp),
    EdbNodeDefinition("EAp", Exp.name),
    EdbFieldDefinition("EAp", "lhs", Exp),
    EdbFieldDefinition("EAp", "rhs", Exp),
    EdbNodeDefinition("ELet", Exp.name),
    EdbFieldDefinition("ELet", "name", TEdbValue(TString)),
    EdbFieldDefinition("ELet", "def", Exp),
    EdbFieldDefinition("ELet", "body", Exp),
    EdbNodeDefinition("ENum", Exp.name),
    EdbFieldDefinition("ENum", "num", TEdbValue(TInt)),
    EdbNodeDefinition("EPlus", Exp.name),
    EdbFieldDefinition("EPlus", "lhs", Exp),
    EdbFieldDefinition("EPlus", "rhs", Exp),
    EdbNodeDefinition("ETrue", Exp.name),
    EdbNodeDefinition("EFalse", Exp.name),
    EdbNodeDefinition("EIf", Exp.name),
    EdbFieldDefinition("EIf", "guard", Exp),
    EdbFieldDefinition("EIf", "lhs", Exp),
    EdbFieldDefinition("EIf", "rhs", Exp),
    EdbNodeDefinition("EPair", Exp.name),
    EdbFieldDefinition("EPair", "lhs", Exp),
    EdbFieldDefinition("EPair", "rhs", Exp),
    EdbNodeDefinition("EProjL", Exp.name),
    EdbFieldDefinition("EProjL", "exp", Exp),
    EdbNodeDefinition("EProjR", Exp.name),
    EdbFieldDefinition("EProjR", "exp", Exp)
  )

  private val Mark = TData("Mark")
  private val MNone = "None"
  private val MFree = "Free"
  private val MLamAnaNonFun = "LamAnaNonFun"
  private val MLamAnaInconAsc = "LamAnaInconAsc"
  private val MApSynNonFun = "ApSynNonFun"
  private val MInconBranches = "InconBranches"
  private val MProjSynNonProd = "ProjSynNonProd"
  private val MInconTypes = "InconTypes"
  contents ++= Seq(
    DataDefinition(Mark.ref.name),
    CaseDefinition(MNone, Seq(), Mark),
    CaseDefinition(MFree, Seq(), Mark),
    CaseDefinition(MLamAnaNonFun, Seq(Type), Mark),
    CaseDefinition(MLamAnaInconAsc, Seq(Type), Mark),
    CaseDefinition(MApSynNonFun, Seq(Type), Mark),
    CaseDefinition(MInconBranches, Seq(Type, Type), Mark),
    CaseDefinition(MProjSynNonProd, Seq(Type), Mark),
    CaseDefinition(MInconTypes, Seq(Type, Type), Mark)
  )

  private val ConstructMNone = Construct(MNone, Seq())

  private val Ctx = TMap(TEdbValue(TString), Type)

  private def ctx = Var("ctx")
  private def e = Var("e")
  private def e(i: Int) = Var(s"e$i")
  private def mark = Var("mark")
  private def mark(i: Int) = Var(s"mark$i")
  private def ty = Var("ty")
  private def ty(i: Int) = Var(s"ty$i")
  private def x = Var("x")

  private val consistent = "consistent"
  contents += Relation(
    consistent,
    Seq(
      Param(ty(1).name, Type),
      Param(ty(2).name, Type)
    ),
    Seq(
      Body( // TCUnknown1
        Seq(Eq(ty(1), ConstructTUnknown))
      ),
      Body( // TCUnknown2
        Seq(Eq(ty(2), ConstructTUnknown))
      ),
      Body( // TCRefl
        Seq(Eq(ty(1), ty(2)))
      ),
      Body( // TCArr
        Seq(
          Deconstruct(ty(1), TArrow, Seq(ty(3).arg, ty(4).arg)),
          Deconstruct(ty(2), TArrow, Seq(ty(5).arg, ty(6).arg)),
          Call(consistent, Seq(ty(3), ty(5))),
          Call(consistent, Seq(ty(4), ty(6)))
        )
      ),
      Body( // TCProd
        Seq(
          Deconstruct(ty(1), TProd, Seq(ty(3).arg, ty(4).arg)),
          Deconstruct(ty(2), TProd, Seq(ty(5).arg, ty(6).arg)),
          Call(consistent, Seq(ty(3), ty(5))),
          Call(consistent, Seq(ty(4), ty(6)))
        )
      )
    )
  )

  private val synMark = "synMark"
  private val anaMark = "anaMark"
  contents += Relation(
    synMark,
    Seq(
      Param(ctx.name, TDemand(Ctx)),
      Param(e.name, TDemand(Exp)),
      Param(mark.name, Mark),
      Param(ty.name, Type)
    ),
    Seq(
      Body( // MKSVar
        EdbDeconstruct(e, "EVar", "name" -> x) ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, MapLookUp(ctx, x))
        )
      ),
      Body( // MKSFree
        EdbDeconstruct(e, "EVar", "name" -> x) ++ Seq(
          Eq(mark, Construct(MFree, Seq())),
          Not(MapContains(ctx, x)),
          Eq(ty, ConstructTUnknown)
        )
      ),
      Body( // MKSNum
        EdbDeconstruct(e, "ENum") ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTNum)
        )
      ),
      Body( // MKSPlus
        EdbDeconstruct(e, "EPlus", "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Deconstruct(ty(1), TNum, Seq()),
          Call(anaMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(2).arg)),
          Deconstruct(ty(2), TNum, Seq()),
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTNum)
        )
      )
    )
  )

  contents += Relation(
    anaMark,
    Seq(
      Param(ctx.name, TDemand(Ctx)),
      Param(e.name, TDemand(Exp)),
      Param(mark.name, Mark),
      Param(ty.name, TDemand(Type))
    ),
    Seq()
  )

  def module: Module = Module(
    "Hazel",
    new Language(
      Set(
        BaseIR,
        arithmetic.IR,
        data.IR,
        demand.IR,
        edbdata.IR,
        map.IR,
        not.IR,
        string.IR
      )
    ),
    contents.toList
  )

object MarkedLambda extends App:
  val module = new MarkedLambda().module
  val compiled = new CompiledHazelModule(module)
  println(module)
  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nChecked:")
  try compiled.checked
  finally println(module)

  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nLowered:")
  println(compiled.lowered)
