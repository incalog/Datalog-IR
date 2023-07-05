package inca.ir.extension.bool

import inca.ir.extension.*
import inca.ir.extension.disjunction.Disjunction
import inca.ir.extensions.{ArithmeticIR, IntNum, Max, Min, Sub}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Eq, Name, Term, Var}


trait Lowering[S <: IR with not.IR, T <: BaseIR with ArithmeticIR] extends not.Lowering[S, T]:

  private var freshCount = 0
  def freshName(): Name =
    val x = IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case BoolAtom(t) =>
      for (v <- visitTerm(t))
        yield Eq(v, IntNum(1))
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case term: BoolTerm => lowerTerm(term)
    case _ => super.visitTerm(term)

  val TrueNum = IntNum(1)
  val FalseNum = IntNum(0)

  def lowerTerm(term: BoolTerm): Seq[Term] = term match
    case AtomAsBool(a) =>
      val x = freshName()
      Seq(
        block.Block(Seq(Disjunction(Seq(
          Seq(a, Eq(Var(x), TrueNum)),
          Seq(negateAtom(a), Eq(Var(x), FalseNum))
        ))), Var(x)))
    case BoolAnd(t1, t2) =>
      for (v1 <- visitTerm(t1); v2 <- visitTerm(t2))
        yield Min(v1, v2)
    case BoolOr(t1, t2) =>
      for (v1 <- visitTerm(t1); v2 <- visitTerm(t2))
        yield Max(v1, v2)
    case BoolNot(t) =>
      for (v <- visitTerm(t))
        yield Sub(IntNum(1), v)
    case BoolTrue => Seq(TrueNum)
    case BoolFalse => Seq(FalseNum)

  override def negateAtom(atom: Atom): Atom = atom match
    case BoolAtom(t) => BoolAtom(BoolNot(t))
    case _ => super.negateAtom(atom)