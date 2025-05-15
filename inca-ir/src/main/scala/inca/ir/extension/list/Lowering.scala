package inca.ir.extension.list

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.data
import inca.ir.extension.data.TData
import inca.ir.extension.list.{IR, ListLit, TList}
import inca.ir.lowering.BaseLowering
import inca.ir.extension.block
import inca.ir.extension.demand
import inca.ir.extension.arithmetic
import inca.ir.extension.block.Block
import inca.ir.extension.bool
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.disjunction
import inca.ir.extension.demand.TDemand
import inca.ir.extension.disjunction.Disjunction

trait Lowering extends BaseLowering:
  override val name: String = "List"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(block.IR, demand.IR, bool.IR, disjunction.IR, arithmetic.IR)

  var listTypes: Set[Type] = Set()

  private def dataName(ty: Type): Name = Name(s"List$$$ty")

  private def appendRelation(ty: Type): Relation =
    Relation(s"List$$$ty$$Append", Seq(
      Param("ls", TDemand(TData(s"List$$$ty"))),
      Param("el", TDemand(ty)),
      Param("out", TData(s"List$$$ty"))
    ), Seq(
      Body(Seq(
        data.Deconstruct(Var("ls"), s"Nil$$$ty", Seq()),
        Eq(Var("out"), data.Construct(s"Cons$$$ty", Seq(
          Var("el"),
          data.Construct(s"Nil$$$ty", Seq())
        )))
      )),
      Body(Seq(
        data.Deconstruct(Var("ls"), s"Cons$$$ty", Seq(Var("curEl"), Var("tl"))),
        Call(s"List$$$ty$$Append", Seq(Var("tl"), Var("el"), Var("tlOut"))),
        Eq(Var("out"), data.Construct(s"Cons$$$ty", Seq(Var("curEl"), Var("tlOut"))))
      ))
    ))

  private def sizeRelation(ty: Type): Relation =
    Relation(s"List$$$ty$$Size", Seq(
      Param("ls", TDemand(TData(s"List$$$ty"))),
      Param("size", TInt)
    ), Seq(
      Body(Seq(
        data.Deconstruct(Var("ls"), s"Nil$$$ty", Seq()),
        Eq(Var("size"), IntNum(0))
      )),
      Body(Seq(
        data.Deconstruct(Var("ls"), s"Cons$$$ty", Seq(WildcardArg(), Var("tl").arg)),
        Call(s"List$$$ty$$Size", Seq(Var("tl"), Var("size$1"))),
        Eq(Var("size"), Add(Var("size$1"), IntNum(1)))
      ))
    ))

  override def visitModule(module: Module): Module =
    val Module(name, language, contents) = super.visitModule(module)

    val listADTs = listTypes.flatMap { ty =>
      val dataDef = data.DataDefinition(dataName(ty))
      val nilCase = data.CaseDefinition(s"Nil$$$ty", Seq(), TData(dataName(ty)))
      val consCase = data.CaseDefinition(s"Cons$$$ty", Seq(ty, TData(dataName(ty))), TData(dataName(ty)))
      Seq(dataDef, nilCase, consCase)
    }.toSeq

    val utilityRels = listTypes.flatMap(ty => Seq(appendRelation(ty), sizeRelation(ty)))
    Module(name, language, listADTs ++ utilityRels ++ contents)

  override def visitType(ty: Type): Type = ty match
    case TList(ty) =>
      listTypes += ty
      TData(dataName(ty))
    case _ =>
      super.visitType(ty)

  private def getListType(listTypeOption: Option[Type]): TList = listTypeOption match
    case Some(listTy@TList(ty)) => listTy
    case Some(ty) => throw IllegalStateException(s"Expected list type, but got $ty")
    case _ => throw IllegalStateException(s"Got none type, but expected a list type.")


  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case Cast(t, ty) =>
        val TermType(_, mode) = t.typ.get
        t.typed(TermType(ty, mode), force = true)
        super.visitTerm(term)
      case ListLit(Seq()) =>
        val listTy = getListType(term.typ.map(_.ty))
        Seq(data.Construct(s"Nil$$${listTy.ty}", Seq()))
      case ListLit(ts) =>
        val listTy = getListType(term.typ.map(_.ty))
        val Seq(visitedHead) = visitTerm(ts.head)
        val remainingList = ListLit(ts.tail)
        remainingList.typed(listTy.bound)
        Seq(data.Construct(s"Cons$$${listTy.ty}", visitedHead +: visitTerm(remainingList)))
      case Head(list) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty
        val headVar = Var(gensym.fresh("hd"))
        visitTerm(list).map { listTerm =>
          block.Block(
            data.Deconstruct(listTerm, s"Cons$$${listTy.ty}", Seq(headVar.arg, WildcardArg())),
            headVar
          )
        }
      case Tail(list) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty
        val tailVar = Var(gensym.fresh("tl"))
        visitTerm(list).map { listTerm =>
          block.Block(
            data.Deconstruct(listTerm, s"Cons$$${listTy.ty}", Seq(WildcardArg(), tailVar.arg)),
            tailVar
          )
        }
      case Size(list) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty
        visitTerm(list).map { listTerm =>
          val outVar = Var(gensym.fresh("size"))
          block.Block(
            Call(s"List$$${listTy.ty}$$Size", Seq(listTerm, outVar)),
            outVar
          )
        }
      case Prepend(list, element) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty
        val visitedElements = visitTerm(element)
        visitTerm(list).flatMap { listTerm =>
          visitedElements.map { el =>
            data.Construct(s"Cons$$${listTy.ty}", el +: Seq(listTerm))
          }
        }
      case Append(list, element) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty
        val visitedElements = visitTerm(element)
        visitTerm(list).flatMap { listTerm =>
          visitedElements.map { el =>
            val outVar = Var(gensym.fresh("ls"))
            block.Block(
              Call(s"List$$${listTy.ty}$$Append", Seq(listTerm, el, outVar)),
              outVar
            )
          }
        }
      case IsEmpty(list) =>
        val listTy = getListType(list.typ.map(_.ty))
        listTypes += listTy.ty

        visitTerm(list).flatMap { listTerm =>
          val isEmptyVar = Var(gensym.fresh("isEmpty"))
          val dis = Disjunction(
            Seq(
              data.Deconstruct(listTerm, s"Nil$$${listTy.ty}", Seq()),
              Eq(isEmptyVar, BoolTrue)
            ),
            Seq(
              data.Deconstruct(listTerm, s"Cons$$${listTy.ty}", Seq(WildcardArg(), WildcardArg())),
              Eq(isEmptyVar, BoolFalse)
            )
          )
          Seq(Block(dis, isEmptyVar))
        }
      case _ => super.visitTerm(term)
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(list, hd, tail) =>
      val listTy = getListType(list.typ.map(_.ty))
      listTypes += listTy.ty
      val Seq(visitedList) = visitTerm(list)
      Seq(data.Deconstruct(visitedList, s"Cons$$${listTy.ty}", Seq(hd.arg, tail.arg)))
    case _ => super.visitAtom(atom)

