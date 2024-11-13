package inca.ir.extension.tuple

import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.util.SourceLocation
import inca.ir.{TAny, Term, TermType, Type}

trait Typechecker extends BaseIRTypechecker:

  override def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit = (ty, outside) match
    case (TTuple(tys1), TTuple(tys2)) =>
      if (tys1.size == tys2.size)
        tys1.zip(tys2).foreach(assertComparable(_, _, t))
      else
        error(s"Cannot compare tuples of different sizes ${tys1.size} and ${tys2.size}", t)
    case _ => super.assertComparable(ty, outside, t)

  protected override def inferTermExtend(term: Term, mode: Mode): TermType = term match {
    case TupleLit(ts) =>
      val (tys, m) = ts.foldRight((List.empty[Type], Mode.Bound)) { case (tt, (tys, m)) =>
        val TermType(tty, ttm) = inferTerm(tt, mode)
        (tty :: tys, m || ttm)
      }
      TermType(TTuple(tys), m)
    case Project(t, idx) => inferTerm(t, Mode.Bound).ty match {
      case TTuple(tys) if 0 <= idx && idx < tys.size => tys(idx).bound
      case TTuple(tys) =>
        error("Projection index out of bounds", term)
        TAny.bound
      case ty =>
        error(s"Expected tuple type but got $ty", term)
        TAny.bound
    }
    case _ => super.inferTermExtend(term, mode)
  }


  protected override def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode = term match
    case TupleLit(ts) => expected match
      case TTuple(ttys) =>
        if (ts.size != ttys.size)
          error(s"Wrong number of tuple elements, expected ${ttys.size} but got ${ts.size}", term)
        var resMode = Mode.Bound
        ts.zip(ttys).foreach { case (t, tty) =>
          resMode = resMode || checkTerm(t, tty, mode)
        }
        resMode
      case _ => super.checkTermExtend(term, expected, mode)
    case Project(t, idx) => inferTerm(t, Mode.Bound).ty match {
      case TTuple(tys) if 0 <= idx && idx < tys.size =>
        assertComparable(tys(idx), expected, term)
        Mode.Bound
      case TTuple(tys) =>
        error("Projection index out of bounds", term)
        Mode.Bound
      case ty =>
        error(s"Expected tuple type but got $ty", term)
        Mode.Bound
    }
    case _ => super.checkTermExtend(term, expected, mode)

  override def checkType(ty: Type): Unit = ty match
    case TTuple(tys) => tys.foreach(checkType)
    case _ => super.checkType(ty)