package inca.ir.extension.list

import inca.ir.*
import inca.ir.extension.list.*
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

trait Visitor extends BaseIRVisitor:

  override def visitTerm(term: Term): Seq[Term] = term match
    case ListLit(ts) => Seq(ListLit(ts.flatMap(visitTerm)))
    case Size(list) => visitTerm(list).map(Size.apply)
    case Head(list) => visitTerm(list).map(Head.apply)
    case Tail(list) => visitTerm(list).map(Tail.apply)
    case IsEmpty(list) => visitTerm(list).map(IsEmpty.apply)
    case Append(list, element) => visitTerm(list).zip(Seq(visitTerm(element).head)).map { case (l, e) => Append(l, e) } // Fügt ein Element an die Liste an
    case Prepend(list, element) => visitTerm(list).zip(Seq(visitTerm(element).head)).map { case (l, e) => Prepend(l, e) } // Fügt ein Element an den Anfang der Liste
    case _ => super.visitTerm(term)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(list, hd, tail) =>
      val Seq(visitedList) = visitTerm(list)
      val Seq(visitedHd) = visitTerm(hd)
      val Seq(visitedTail) = visitTerm(tail)
      Seq(Deconstruct(visitedList, visitedHd, visitedTail))
    case _ => super.visitAtom(atom)

  override def visitType(ty: Type): Type = ty match
    case TList(ty) => TList(visitType(ty))
    case _ => super.visitType(ty)
