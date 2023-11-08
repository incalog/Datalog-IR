package inca.ir.extension.bool

import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{IntNum, Max, Min, Sub, TInt}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.not.Not
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Eq, Name, Term, Type, Var}

trait Lowering extends not.Lowering:
  override val loweredIRs: Set[BaseIR] = Set(IR) ++ super.loweredIRs
  override val requiredIRs: Set[BaseIR] = Set(arithmetic.IR, block.IR, disjunction.IR, not.IR) ++ super.requiredIRs

  private def freshName(): Name = gensym.freshName(Name(IR.name))

  override def visitAtom(atom: Atom): Seq[Atom] =  preserveHints(atom)(atom match
    case BoolAtom(t) =>
      for (v <- visitTerm(t))
        yield Eq(v, TrueNum)
    case _ => super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case term: BoolTerm => lowerTerm(term)
      case _ => super.visitTerm(term)
  }

  val TrueNum = IntNum(1)
  val FalseNum = IntNum(0)

  def lowerTerm(term: BoolTerm): Seq[Term] = term match
    case AtomAsBool(a) =>
      val x = freshName()
      Seq(
        block.Block(Seq(Disjunction(Seq(
          DisjunctionAlternative(a, Eq(Var(x), TrueNum)),
          DisjunctionAlternative(Not(a), Eq(Var(x), FalseNum))
        ))), Var(x)))
    case BoolAnd(t1, t2) =>
      visitTerm(t1).zip(visitTerm(t2)).map(Min.apply)
    case BoolOr(t1, t2) =>
      visitTerm(t1).zip(visitTerm(t2)).map(Max.apply)
    case BoolNot(t) =>
      for (v <- visitTerm(t))
        yield Sub(IntNum(1), v)
    case BoolTrue => Seq(TrueNum)
    case BoolFalse => Seq(FalseNum)

  override def negateAtom(atom: Atom): Atom = preserveHints(atom) {
    atom match
      case BoolAtom(t) => BoolAtom(BoolNot(t))
      case _ => super.negateAtom(atom)
  }

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TBoolean => TInt
      case _ => super.visitType(ty)
  }