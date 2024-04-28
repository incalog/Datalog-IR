package inca.ir.valueNumbering

import inca.ir.{Atom, Name, RefByName, TAny, Term, TermType, Type, Var}
import inca.ir.extension.arithmetic.*
import inca.ir.typing.Mode.Bound


trait ArithmeticValueNumbering(config: ConfigVN = ConfigVN()) extends BaseValueNumbering {

  protected override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

  // in the beginning no recursive call needed here since called in visitTerm ->  all subterms visited already
  protected override def normalize(term: Term): Term = {
    if !this.config.normalize then return term
    val typ: Type = term.typ match { // assumed that program was typechecked before and every term thus has a type
      case Some(termType: TermType) => termType.ty
      case _ => TAny // below only tested whether TInt or TDouble
    }
    val newTerm = term match {
      case BinOp(lhs, rhs, "+") => normalizeAdd(lhs, rhs, typ)
      case BinOp(lhs, rhs, "-") => normalizeSub(lhs, rhs, typ)
      case BinOp(lhs, rhs, "*") => normalizeMul(lhs, rhs, typ)
      case BinOp(lhs, rhs, "/") => normalizeDiv(lhs, rhs, typ)
      case BinOp(lhs, rhs, "%") => normalizeRemainder(lhs,rhs,typ)
      case BinOp(lhs, rhs, "min") => normalizeMin(lhs,rhs,typ)
      case BinOp(lhs, rhs, "max") => normalizeMax(lhs,rhs,typ)
      case UnOp(t, "abs") => normalizeAbs(t,typ)
      case _ => super.normalize(term)
    }
    newTerm.typ = term.typ
    newTerm
  }

  private def getArgumentsOfOp(lhs: Term, rhs: Term): (Term, Term) = {
    val newLhs = getArgumentOfOp(lhs) // TODO otherwise defterm might contain removed Var... -> other solution? update defterm here?
    val newRhs = getArgumentOfOp(rhs)
    return (newLhs, newRhs)
  }
  private def getArgumentOfOp(t: Term): Term = {
    return visitTerm(getDefiningTerm(t)).head
  }

  private def normalizeAdd(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs,rhs) match {
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => rhs
      case (IntNum(l), IntNum(r)) => IntNum(l + r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l + r)

      case (IntNum(l), BinOp(IntNum(r), restTerm, "+")) => associativityInt(l,r,restTerm,_+_,Add)
      case (BinOp(IntNum(l), restTerm, "+"), IntNum(r)) => associativityInt(l,r,restTerm,_+_,Add)
      case (DoubleNum(l), BinOp(DoubleNum(r), restTerm, "+")) => associativityDouble(l,r,restTerm,_+_,Add)
      case (BinOp(DoubleNum(l), restTerm, "+"), DoubleNum(r)) => associativityDouble(l,r,restTerm,_+_,Add)

      case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ == TInt => normalize(Mul(IntNum(2), lhs)) // x + x == 2*x for Ints
      case (lhs, BinOp(IntNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(IntNum(x + 1), lhs))
      case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ == TDouble => normalize(Mul(DoubleNum(2), lhs)) // for Doubles
      case (lhs, BinOp(DoubleNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(DoubleNum(x + 1), lhs))

      case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then Add(lvar,rvar) else BinOp(rvar, lvar, "+") // this and following five for commutativity & associativity
      case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "+")
      case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "+")) => BinOp(lNum, normalize(BinOp(lhs, rterm, "+")), "+")
      case (lvar@Var(RefByName(Name(nameL))), BinOp(rvar@Var(RefByName(Name(nameR))), rterm, "+")) if nameL > nameR =>
        BinOp(rvar, normalize(BinOp(lvar, rterm, "+")), "+")
      case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getIdOf(lBinOp) > getIdOf(rBinOp) then Add(rBinOp, lBinOp) else Add(lBinOp,rBinOp)
      case (lBinOp@BinOp(_, _, _), rhs) => normalize(BinOp(rhs, lBinOp, "+"))
      case (l, r) => Add(l,r)
    }

  private def normalizeSub(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs,rhs) match { // TODO change to add (?)
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => normalize(Mul(rhs, IntNum(-1)))
//      case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(0) else if typ == TDouble then DoubleNum(0) else term
      case (l, r) if getIdOf(l) == getIdOf(r) => if typ == TInt then IntNum(0) else if typ == TDouble then DoubleNum(0) else Sub(l,r)
      case (IntNum(l), IntNum(r)) => IntNum(l - r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l - r)

      case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => normalize(BinOp(IntNum(l - r), normalize(Mul(rvar, IntNum(-1))), "+"))
      case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "+")) => normalize(BinOp(DoubleNum(l - r), normalize(Mul(rvar, DoubleNum(-1))), "+"))

      case (lTerm, rBinOp@BinOp(IntNum(r), rTerm, "+")) => if getIdOf(lTerm) == getIdOf(rTerm) then IntNum(-r) else Sub(lTerm,rBinOp)
      case (lTerm, rBinOp@BinOp(DoubleNum(r), rTerm, "+")) => if getIdOf(lTerm) == getIdOf(rTerm) then DoubleNum(-r) else Sub(lTerm,rBinOp)

      case (BinOp(l,r,"+"),rhs) if getIdOf(l) == getIdOf(rhs) => r
      case (BinOp(l,r,"+"),rhs) if getIdOf(r) == getIdOf(rhs) => l

      case (l, r) => Sub(l,r)
    }

  private def normalizeMul(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(0)) | (IntNum(0), _) => IntNum(0)
    case (_, DoubleNum(0)) | (DoubleNum(0), _) => DoubleNum(0)
    case (IntNum(1), _) | (DoubleNum(1), _) => rhs
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
    case (IntNum(l), IntNum(r)) => IntNum(l * r)
    case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l * r)

    case (IntNum(l), BinOp(IntNum(r), restTerm, "*")) => associativityInt(l,r,restTerm,_*_,Mul)
    case (BinOp(IntNum(l), restTerm, "*"), IntNum(r)) => associativityInt(l,r,restTerm,_*_,Mul)
    case (DoubleNum(l), BinOp(DoubleNum(r), restTerm, "*")) => associativityDouble(l,r,restTerm,_*_,Mul)
    case (BinOp(DoubleNum(l), restTerm, "*"), DoubleNum(r)) => associativityDouble(l,r,restTerm,_*_,Mul)

    case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then Mul(lvar,rvar) else BinOp(rvar, lvar, "*") // this and following five for commutativity & associativity
    case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "*")
    case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "*")) => normalize(BinOp(lNum, normalize(BinOp(lhs, rterm, "*")), "*"))
    case (lvar@Var(RefByName(Name(nameL))), BinOp(rvar@Var(RefByName(Name(nameR))), rterm, "*")) if nameL > nameR =>
      BinOp(rvar, normalize(BinOp(lvar, rterm, "*")), "*")
    case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getIdOf(lBinOp) > getIdOf(rBinOp) then Mul(rBinOp, lBinOp) else Mul(lBinOp,rBinOp)
    case (lBinOp@BinOp(_, _, _), rhs) => normalize(BinOp(rhs, lBinOp, "*"))

    case (factor, BinOp(lhs, rhs, "+")) => distributivity(factor,lhs,rhs,Add,Mul)
    case (factor, BinOp(lhs, rhs, "-")) => distributivity(factor,lhs,rhs,Sub,Mul)

    case (lTerm, BinOp(lhs, rBinOp@rTerm, "/")) => if getIdOf(lTerm) == getIdOf(rTerm) then lhs else Mul(lTerm,rBinOp)
    case (l, r) => Mul(l,r)
  }

  private def normalizeDiv(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
//    case (vari@Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else term
    case (l, r) if (getIdOf(l) == getIdOf(r) && getReplacementTerm(r) != IntNum(0) && getReplacementTerm(r) != DoubleNum(0)) =>
      if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else Div(l,r) // TODO 0/0 -> 1
    case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // int/int yields int in scala
    case (DoubleNum(l), DoubleNum(r)) if r != 0 => DoubleNum(l / r)

    case (lBinOp@BinOp(lhs, lTerm, "*"), rTerm) => if getIdOf(lTerm) == getIdOf(rTerm) then lhs else Div(lBinOp,rTerm)
    case (lTerm, lBinOp@BinOp(lhs, rTerm, "*")) if getIdOf(lTerm) == getIdOf(rTerm) =>
      if typ == TInt then normalize(Div(IntNum(1), lhs)) else if typ == TDouble then normalize(Div(DoubleNum(1), lhs)) else Div(lTerm,lBinOp)

    case (BinOp(lhs, rhs, "+"),denom) => distributivity(denom,lhs,rhs,Add,Div)
    case (BinOp(lhs, rhs, "-"),denom) => distributivity(denom,lhs,rhs,Sub,Div)

    case (l, r) => Div(l,r)
  }

  private def normalizeRemainder(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
    case (IntNum(0),_) => IntNum(0)
    case (DoubleNum(0),_) => DoubleNum(0)
    case (IntNum(l), IntNum(r)) => IntNum(l % r)
    case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l % r)
    case (lhs,rhs) if getIdOf(lhs) == getIdOf(rhs) => if typ == TInt then IntNum(0) else DoubleNum(0)
    case (BinOp(l,r,"*"),rhs) if getIdOf(l) == getIdOf(rhs) => if typ == TInt then IntNum(0) else DoubleNum(0)
    case (BinOp(l,r,"*"),rhs) if getIdOf(r) == getIdOf(rhs) => if typ == TInt then IntNum(0) else DoubleNum(0)
    case (l, r) => Remainder(l,r)
  }

  private def normalizeMin(lhs: Term, rhs: Term, typ: Type): Term = getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l < r then DoubleNum(l) else DoubleNum(r)
    case (lvar, IntNum(r)) => Min(IntNum(r), lvar)
    case (lvar, DoubleNum(r)) => Min(DoubleNum(r), lvar)
    case (l, r) => Min(l,r)
  }

  private def normalizeMax(lhs: Term, rhs: Term, typ: Type): Term =  getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l > r then DoubleNum(l) else DoubleNum(r)
    case (l, r) if typ == TInt => Mul(IntNum(-1), normalize(Min(normalize(Mul(IntNum(-1), l)), normalize(Mul(IntNum(-1), r)))))
    case (l, r) if typ == TDouble => Mul(DoubleNum(-1), normalize(Min(normalize(Mul(DoubleNum(-1), l)), normalize(Mul(DoubleNum(-1), r)))))
    //        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
    //        case (lvar, DoubleNum(r)) => simplify(Max(DoubleNum(r), lvar))
    case (l, r) => Max(l,r)
  }

  private def normalizeAbs(t: Term, typ: Type): Term = getArgumentOfOp(t) match {
    case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
    case DoubleNum(value) => if value >= 0 then DoubleNum(value) else DoubleNum(-1 * value)
    case arg => Abs(arg)
  }

  private def associativityInt(l: Int, r: Int, restTerm: Term, intOp: (Int, Int) => Int, op: (Term, Term) => BinOp): Term =
    op(IntNum(intOp(l, r)), restTerm)
  private def associativityDouble(l: Double, r: Double, restTerm: Term, doubleOp: (Double, Double) => Double, op: (Term, Term) => BinOp): Term =
    op(DoubleNum(doubleOp(l, r)), restTerm)
  private def distributivity(factor: Term, lhs: Term, rhs: Term, opOuter: (Term, Term) => BinOp, opInner: (Term, Term) => BinOp): Term =
    normalize(opOuter(normalize(opInner(lhs,factor)), normalize(opInner(rhs,factor))))


}
