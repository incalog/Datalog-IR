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
import inca.viatra.Executor
import inca.viatra.runtime.context.DataModel

import scala.collection.mutable.ListBuffer

class MarkedLambda:
  private val contents: ListBuffer[ModuleEntry] = ListBuffer.empty

  private val Type = TData("Type")
  private val TUnknown = "Unknown"
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
  private val ConstructTArrow = (ty1: Term, ty2: Term) =>
    Construct(TArrow, Seq(ty1, ty2))
  private val ConstructTProd = (ty1: Term, ty2: Term) =>
    Construct(TProd, Seq(ty1, ty2))

  private val TypeAnno = TEdbNode(q("TypeAnno"))
  contents ++= EdbDataModuleEntry.fromNodeMetaInfos(edb.typeAnnoNodes)

  private val Exp = TEdbNode(q("Exp"))
  contents ++= EdbDataModuleEntry.fromNodeMetaInfos(edb.expNodes)

  private val Mark = TData("Mark")
  private val MNone = "None"
  private val MFree = "Free"
  private val MLamAnaNonFun = "LamAnaNonFun"
  private val MLamAnaInconAsc = "LamAnaInconAsc"
  private val MApSynNonFun = "ApSynNonFun"
  private val MInconBranches = "InconBranches"
  private val MPairAnaNonProd = "PairAnaNonProd"
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
    CaseDefinition(MPairAnaNonProd, Seq(Type), Mark),
    CaseDefinition(MProjSynNonProd, Seq(Type), Mark),
    CaseDefinition(MInconTypes, Seq(Type, Type), Mark)
  )

  private val ConstructMNone = Construct(MNone, Seq())

  private val Ctx = TMap(TString, Type)

  private def ctx = Var("ctx")
  private def e = Var("e")
  private def e(i: Int) = Var(s"e$i")
  private def mark = Var("mark")
  private def mark(i: Int) = Var(s"mark$i")
  private def ty = Var("ty")
  private def ty(i: Int) = Var(s"ty$i")
  private def x = Var("x")
  private def xStr = Var("xStr")

  def q(name: String): String = s"inca.hazel.edb.$name"

  private val typeOfEdbType = "typeOfEdbType"
  contents += Relation(
    typeOfEdbType,
    Seq(Param(ty(1).name, TypeAnno), Param(ty(2).name, Type)),
    Seq(
      Body(
        EdbDeconstruct(ty(1), q("TAUnknown")) ++ Seq(
          Eq(ty(2), Construct(TUnknown, Seq()))
        )
      ),
      Body(
        EdbDeconstruct(ty(1), q("TANum")) ++ Seq(
          Eq(ty(2), Construct(TNum, Seq()))
        )
      ),
      Body(
        EdbDeconstruct(ty(1), q("TABool")) ++ Seq(
          Eq(ty(2), Construct(TBool, Seq()))
        )
      ),
      Body(
        EdbDeconstruct(
          ty(1),
          q("TAArrow"),
          "dom" -> Var("tadom"),
          "codom" -> Var("tacodom")
        ) ++ Seq(
          Call(typeOfEdbType, Seq(Var("tadom"), Var("tdom"))),
          Call(typeOfEdbType, Seq(Var("tacodom"), Var("tcodom"))),
          Eq(ty(2), Construct(TArrow, Seq(Var("tdom"), Var("tcodom"))))
        )
      ),
      Body(
        EdbDeconstruct(
          ty(1),
          q("TAProd"),
          "fst" -> Var("tafst"),
          "snd" -> Var("tasnd")
        ) ++ Seq(
          Call(typeOfEdbType, Seq(Var("tafst"), Var("tfst"))),
          Call(typeOfEdbType, Seq(Var("tasnd"), Var("tsnd"))),
          Eq(ty(2), Construct(TProd, Seq(Var("tfst"), Var("tsnd"))))
        )
      )
    )
  )

  private val consistent = "consistent"
  contents += Relation(
    consistent,
    Seq(
      Param(ty(1).name, TDemand(Type)),
      Param(ty(2).name, TDemand(Type))
    ),
    Seq(
      Body( // TCUnknown1
        Seq(Deconstruct(ty(1), TUnknown, Seq()))
      ),
      Body( // TCUnknown2
        Seq(Deconstruct(ty(2), TUnknown, Seq()))
      ),
      Body( // TCRefl
        Seq(Eq(ty(1), ty(2)))
      ),
      Body( // TCArr
        Seq(
          Deconstruct(ty(1), TArrow, Seq(ty(3), ty(4))),
          Deconstruct(ty(2), TArrow, Seq(ty(5), ty(6))),
          Call(consistent, Seq(ty(3), ty(5))),
          Call(consistent, Seq(ty(4), ty(6)))
        )
      ),
      Body( // TCProd
        Seq(
          Deconstruct(ty(1), TProd, Seq(ty(3), ty(4))),
          Deconstruct(ty(2), TProd, Seq(ty(5), ty(6))),
          Call(consistent, Seq(ty(3), ty(5))),
          Call(consistent, Seq(ty(4), ty(6)))
        )
      )
    )
  )

  private val matchedArrow = "matchedArrow"
  contents += Relation(
    matchedArrow,
    Seq(
      Param(ty.name, TDemand(Type)),
      Param(ty(1).name, Type),
      Param(ty(2).name, Type)
    ),
    Seq(
      Body( // TMAUnknown
        Seq(
          Deconstruct(ty, TUnknown, Seq()),
          Eq(ty(1), ConstructTUnknown),
          Eq(ty(2), ConstructTUnknown)
        )
      ),
      Body( // TMAArr
        Seq(
          Deconstruct(ty, TArrow, Seq(ty(1), ty(2)))
        )
      )
    )
  )

  private val matchedProd = "matchedProd"
  contents += Relation(
    matchedProd,
    Seq(
      Param(ty.name, TDemand(Type)),
      Param(ty(1).name, Type),
      Param(ty(2).name, Type)
    ),
    Seq(
      Body( // TMPUnknown
        Seq(
          Deconstruct(ty, TUnknown, Seq()),
          Eq(ty(1), ConstructTUnknown),
          Eq(ty(2), ConstructTUnknown)
        )
      ),
      Body( // TMPProd
        Seq(
          Deconstruct(ty, TProd, Seq(ty(1), ty(2)))
        )
      )
    )
  )

  private val meet = "meet"
  contents += Relation(
    meet,
    Seq(
      Param(ty(1).name, TDemand(Type)),
      Param(ty(2).name, TDemand(Type)),
      Param(ty(3).name, Type)
    ),
    Seq(
      Body(
        Seq(Deconstruct(ty(1), TUnknown, Seq()), Eq(ty(3), ty(2)))
      ),
      Body(
        Seq(Deconstruct(ty(2), TUnknown, Seq()), Eq(ty(3), ty(1)))
      ),
      Body(
        Seq(
          Deconstruct(ty(1), TNum, Seq()),
          Deconstruct(ty(2), TNum, Seq()),
          Eq(ty(3), ConstructTNum)
        )
      ),
      Body(
        Seq(
          Deconstruct(ty(1), TBool, Seq()),
          Deconstruct(ty(2), TBool, Seq()),
          Eq(ty(3), ConstructTBool)
        )
      ),
      Body(
        Seq(
          Deconstruct(ty(1), TArrow, Seq(ty(3), ty(4))),
          Deconstruct(ty(2), TArrow, Seq(ty(5), ty(6))),
          Call(meet, Seq(ty(3).arg, ty(5).arg, ty(7))),
          Call(meet, Seq(ty(4).arg, ty(6).arg, ty(8))),
          Eq(ty(3), ConstructTArrow(ty(7), ty(8)))
        )
      ),
      Body(
        Seq(
          Deconstruct(ty(1), TProd, Seq(ty(3), ty(4))),
          Deconstruct(ty(2), TProd, Seq(ty(5), ty(6))),
          Call(meet, Seq(ty(3).arg, ty(5).arg, ty(7))),
          Call(meet, Seq(ty(4).arg, ty(6).arg, ty(8))),
          Eq(ty(3), ConstructTProd(ty(7), ty(8)))
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
        EdbDeconstruct(e, q("EVar"), "name" -> x) ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, MapLookUp(ctx, Cast(x, TString)))
        )
      ),
      Body( // MKSFree
        EdbDeconstruct(e, q("EVar"), "name" -> x) ++ Seq(
          Eq(mark, Construct(MFree, Seq())),
          // TODO: Not(MapContains(ctx, x)), creates a negative cycle
          Eq(IntNum(0), IntNum(1)), // TODO dummy constraint that always fails
          Eq(ty, ConstructTUnknown)
        )
      ),
      Body( // MKSLam
        EdbDeconstruct(
          e,
          q("ELam"),
          "param" -> x,
          "ty" -> ty(1),
          "body" -> e(1)
        ) ++ Seq(
          Call(typeOfEdbType, Seq(ty(1).arg, ty(2).arg)),
          Eq(xStr, Cast(x, TString)),
          Call(
            synMark,
            Seq(MapPlus(ctx, xStr, ty(2)).arg, e(1).arg, mark(1).arg, ty(3).arg)
          ),
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTArrow(ty(2), ty(3)))
        )
      ),
      Body( // MKSAp1
        EdbDeconstruct(
          e,
          q("EAp"),
          "lhs" -> e(1),
          "rhs" -> e(2)
        ) ++ Seq(
          Call(
            synMark,
            Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)
          ),
          Call(matchedArrow, Seq(ty(1).arg, ty(2).arg, ty.arg)),
          Call(
            anaMark,
            Seq(ctx.arg, e(2).arg, mark(2).arg, ty(2).arg)
          ),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKSAp2
        EdbDeconstruct(
          e,
          q("EAp"),
          "lhs" -> e(1),
          "rhs" -> e(2)
        ) ++ Seq(
          Call(
            synMark,
            Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)
          ),
          // Not(Call(matchedArrow, Seq(ty(1).arg, ty(2).arg, ty.arg))),
          Call(
            anaMark,
            Seq(ctx.arg, e(2).arg, mark(2).arg, ConstructTUnknown)
          ),
          Eq(mark, Construct(MApSynNonFun, Seq(ty(1)))),
          Eq(ty, ConstructTUnknown)
        )
      ),
      Body( // MKSLet
        EdbDeconstruct(
          e,
          q("ELet"),
          "name" -> x,
          "defn" -> e(1),
          "body" -> e(2)
        ) ++ Seq(
          Call(
            synMark,
            Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)
          ),
          Eq(xStr, Cast(x, TString)),
          Call(
            synMark,
            Seq(MapPlus(ctx, xStr, ty(1)).arg, e(2).arg, mark(2).arg, ty.arg)
          ),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKSNum
        EdbDeconstruct(e, q("ENum")) ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTNum)
        )
      ),
      Body( // MKSPlus
        EdbDeconstruct(e, q("EPlus"), "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ConstructTNum)),
          Call(anaMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ConstructTNum)),
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTNum)
        )
      ),
      Body( // MKSTrue
        EdbDeconstruct(e, q("ETrue")) ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTBool)
        )
      ),
      Body( // MKSFalse
        EdbDeconstruct(e, q("EFalse")) ++ Seq(
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTBool)
        )
      ),
      Body( // MKSIf
        EdbDeconstruct(
          e,
          q("EIf"),
          "guard" -> e(1),
          "lhs" -> e(2),
          "rhs" -> e(3)
        ) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ConstructTBool)),
          Call(synMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(1).arg)),
          Call(synMark, Seq(ctx.arg, e(3).arg, mark(3).arg, ty(2).arg)),
          Call(meet, Seq(ty(1).arg, ty(2).arg, ty.arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKSInconsistentBranches
        EdbDeconstruct(
          e,
          q("EIf"),
          "guard" -> e(1),
          "lhs" -> e(2),
          "rhs" -> e(3)
        ) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ConstructTBool)),
          Call(synMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(1).arg)),
          Call(synMark, Seq(ctx.arg, e(3).arg, mark(3).arg, ty(2).arg)),
          Eq(mark, Construct(MInconBranches, Seq(ty(1), ty(2)))),
          Eq(ty, ConstructTUnknown)
        )
      ),
      Body( // MKSPair
        EdbDeconstruct(e, q("EPair"), "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
          Call(synMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Call(synMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(2).arg)),
          Eq(mark, ConstructMNone),
          Eq(ty, ConstructTProd(ty(1), ty(2)))
        )
      ),
      Body( // MKSProjL1
        EdbDeconstruct(e, q("EProjL"), "exp" -> e(1)) ++ Seq(
          Call(synMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Call(matchedProd, Seq(ty(1).arg, ty.arg, ty(2).arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKSProjL2
        EdbDeconstruct(e, q("EProjL"), "exp" -> e(1)) ++ Seq(
          Call(synMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Eq(mark, Construct(MProjSynNonProd, Seq(ty(1)))),
          Eq(ty, ConstructTUnknown)
        )
      ),
      Body( // MKSProjR1
        EdbDeconstruct(e, q("EProjR"), "exp" -> e(1)) ++ Seq(
          Call(synMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Call(matchedProd, Seq(ty(1).arg, ty(2).arg, ty.arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKSProjR2
        EdbDeconstruct(e, q("EProjR"), "exp" -> e(1)) ++ Seq(
          Call(synMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Eq(mark, Construct(MProjSynNonProd, Seq(ty(1)))),
          Eq(ty, ConstructTUnknown)
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
    Seq(
      Body( // MKALam1
        EdbDeconstruct(
          e,
          q("ELam"),
          "param" -> x,
          "ty" -> ty(1),
          "body" -> e(1)
        ) ++ Seq(
          Call(matchedArrow, Seq(ty.arg, ty(4).arg, ty(5).arg)),
          Call(typeOfEdbType, Seq(ty(1).arg, ty(2).arg)),
          Call(consistent, Seq(ty(2).arg, ty(4).arg)),
          Eq(xStr, Cast(x, TString)),
          Call(
            anaMark,
            Seq(MapPlus(ctx, xStr, ty(2)).arg, e(1).arg, mark(1).arg, ty(5).arg)
          ),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKALam3
        EdbDeconstruct(
          e,
          q("ELam"),
          "param" -> x,
          "ty" -> ty(1),
          "body" -> e(1)
        ) ++ Seq(
          Call(matchedArrow, Seq(ty.arg, ty(4).arg, ty(5).arg)),
          Call(typeOfEdbType, Seq(ty(1).arg, ty(2).arg)),
          Eq(xStr, Cast(x, TString)),
          Call(
            anaMark,
            Seq(MapPlus(ctx, xStr, ty(2)).arg, e(1).arg, mark(1).arg, ty(5).arg)
          ),
          Eq(mark, Construct(MLamAnaInconAsc, Seq(ty(4))))
        )
      ),
      Body( // MKALam2
        EdbDeconstruct(
          e,
          q("ELam"),
          "param" -> x,
          "ty" -> ty(1),
          "body" -> e(1)
        ) ++ Seq(
          Call(typeOfEdbType, Seq(ty(1).arg, ty(2).arg)),
          Eq(xStr, Cast(x, TString)),
          Call(
            anaMark,
            Seq(
              MapPlus(ctx, xStr, ty(2)).arg,
              e(1).arg,
              mark(1).arg,
              ConstructTUnknown
            )
          ),
          Eq(mark, Construct(MLamAnaNonFun, Seq(ty)))
        )
      ),
      Body( // MKALet
        EdbDeconstruct(
          e,
          q("ELet"),
          "name" -> x,
          "defn" -> e(1),
          "body" -> e(2)
        ) ++ Seq(
          Call(
            synMark,
            Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)
          ),
          Eq(xStr, Cast(x, TString)),
          Call(
            anaMark,
            Seq(MapPlus(ctx, xStr, ty(1)).arg, e(2).arg, mark(2).arg, ty.arg)
          ),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKAIf
        EdbDeconstruct(
          e,
          q("EIf"),
          "guard" -> e(1),
          "lhs" -> e(2),
          "rhs" -> e(3)
        ) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ConstructTBool)),
          Call(anaMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty.arg)),
          Call(anaMark, Seq(ctx.arg, e(3).arg, mark(3).arg, ty.arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKAPair1
        EdbDeconstruct(e, q("EPair"), "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
          Call(matchedProd, Seq(ty.arg, ty(1).arg, ty(2).arg)),
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ty(1).arg)),
          Call(anaMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ty(2).arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKAPair2
        EdbDeconstruct(e, q("EPair"), "lhs" -> e(1), "rhs" -> e(2)) ++ Seq(
          Call(anaMark, Seq(ctx.arg, e(1).arg, mark(1).arg, ConstructTUnknown)),
          Call(anaMark, Seq(ctx.arg, e(2).arg, mark(2).arg, ConstructTUnknown)),
          Eq(mark, Construct(MPairAnaNonProd, Seq(ty)))
        )
      ),
      Body( // MKASubsume
        Seq(
          Call(synMark, Seq(ctx.arg, e.arg, mark(1).arg, ty(1).arg)),
          Call(consistent, Seq(ty.arg, ty(1).arg)),
          Eq(mark, ConstructMNone)
        )
      ),
      Body( // MKAInconsistentTypes
        Seq(
          Call(synMark, Seq(ctx.arg, e.arg, mark(1).arg, ty(1).arg)),
          Eq(mark, Construct(MInconTypes, Seq(ty, ty(1))))
        )
      )
    )
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
  private val module = new MarkedLambda().module
  private val compiled = new CompiledHazelModule(module)
  println(module)
  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nChecked:")
  try compiled.checked
  finally println(module)

  println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\nLowered:")
  println(compiled.lowered)

  val dataModel = DataModel.from(edb.allNodes: _*)

  val exec = new Executor()
  val engine = exec.instantiate(compiled, dataModel)

  {
    import edb.*
    val t1 = TAArrow(TANum(), TANum())
    val t2 = TAArrow(TABool(), TANum())
    val t3 = TAArrow(TAUnknown(), TANum())

    println(s"Loading $t1")
    t1.loadEdits.print()
    engine.feed.processEditScript(t1.loadEdits)
    engine.readAll().map(_.asTable).foreach(println)

    println(s"Replace by $t2")
    val (edits12, newT2) = t1.compareTo(t2)
    edits12.print()
    engine.feed.processEditScript(edits12)
    engine.readAll().map(_.asTable).foreach(println)

    println(s"Replace by $t3")
    val (edits23, newT3) = newT2.compareTo(t3)
    edits23.print()
    engine.feed.processEditScript(edits23)
    engine.readAll().map(_.asTable).foreach(println)
  }

// negative cycle
// synMark -> Map$TString@edb_Type$$rel -> Map$TString@edb_Type$$rel$input -> synMark$input -> anaMark -> synMark
