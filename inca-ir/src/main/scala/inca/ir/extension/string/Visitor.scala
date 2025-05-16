package inca.ir.extension.string

import inca.ir.Hint.preserveHints
import inca.ir.extension.not
import inca.ir.visitors.BaseIRVisitor
import inca.ir.{Atom, Term, Type}

trait Visitor extends BaseIRVisitor with not.Visitor:
  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case RegexMatch(t, pattern, neg) =>
      visitTerm(t).zip(visitTerm(pattern)).map((t1, p) => RegexMatch(t1, p, neg))
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case StringLit(s) => Seq(StringLit(s))
    case StringConcat(lhs, rhs) =>
      visitTerm(lhs).zip(visitTerm(rhs)).map(StringConcat.apply)
    case ToString(t) =>
      visitTerm(t).map(ToString.apply)
    case Substring(t, index, length) =>
      for {
        tt <- visitTerm(t)
        i <- visitTerm(index)
        l <- visitTerm(length)
      } yield Substring(tt, i, l)
    case StringLength(t) =>
      visitTerm(t).map(StringLength.apply)
    case OrdinalNumber(t) =>
      visitTerm(t).map(OrdinalNumber.apply)
    case _ => super.visitTerm(term))

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TString => TString
    case _ => super.visitType(ty))

  override def negateAtom(atom: Atom): Atom = atom match
    case RegexMatch(t, pattern, neg) => RegexMatch(t, pattern, !neg)
    case _ => super.negateAtom(atom)