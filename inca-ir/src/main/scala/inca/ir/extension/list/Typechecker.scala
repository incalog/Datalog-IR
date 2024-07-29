package inca.ir.extension.list

import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.list.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.{Atom, TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:

   override def checkType(ty: Type): Unit = ty match
      case TList(ty) => checkType(ty)
      case _ => super.checkType(ty)

   private def inferListTerm(list: Term): TList = inferTerm(list, Mode.Bound).ty match
      case TList(ty) => TList(ty)
      case ty =>
         error(s"List operation is not defined for non-list type $ty", list)
         TList(TAny)

   protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match
      case ListLit(ts) if ts.nonEmpty =>
         val firstType = inferTerm(ts.head, Mode.Bound).ty
         ts.tail.foreach(t => checkTerm(t, firstType, Mode.Bound))
         TList(firstType).bound
      case ListLit(ts) if ts.isEmpty =>
         TList(TAny).bound
      case Size(list) =>
         inferListTerm(list)
         TInt.bound
      case Head(list) =>
         val listTy = inferListTerm(list)
         listTy.ty.bound
      case Tail(list) =>
         val listTy = inferListTerm(list)
         listTy.bound
      case Append(list, element) =>
         val listTy = inferListTerm(list)
         checkTerm(element, listTy.ty, Mode.Bound)
         listTy.bound
      case Prepend(list, element) =>
         val listTy = inferListTerm(list)
         checkTerm(element, listTy.ty, Mode.Bound)
         listTy.bound
      case IsEmpty(list) =>
         inferListTerm(list)
         TBoolean.bound
      case _ => super.inferTermExtend(term, mode)
   
   override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
      case Deconstruct(list, hd, tail) =>
         val listTy = inferListTerm(list)
         checkTerm(hd, listTy.ty, mode)
         checkTerm(tail, listTy, mode)
      case _ => super.checkAtom(atom, mode)


