package inca.ir.valueNumbering

import inca.ir.{Atom, Name, RefByName, TAny, Term, TermType, Type, Var}
import inca.ir.extension.arithmetic.*
import inca.ir.typing.Mode.Bound

trait ArithmeticValueNumbering(config: ConfigVN) extends BaseValueNumbering {

//  protected override def getHashCode(atom: Atom): ValueId = atom match{
//    case BinCompare(lhs, rhs, op) => Seq(BinCompare,getHashCode(lhs),getHashCode(rhs),op).hashCode()
//    case _ => super.getHashCode(atom)
//  }
//
//  protected override def getHashCode(term: Term): ValueId = term match {
//    case BinOp(lhs,rhs,op) => Seq(BinOp,getHashCode(lhs),getHashCode(rhs),op).hashCode()
//    case UnOp(t, op) => Seq(UnOp,getHashCode(t),op).hashCode()
//    case IntNum(_) | DoubleNum(_) => super.getHashCode(term)
//    case _ => super.getHashCode(term)
//  }

  protected override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

//  protected override def removeAtomIfTrue(newAtomSeq: Seq[Atom]): Seq[Atom] = {
//    if newAtomSeq.isEmpty || !config.removeTrueAtoms then return newAtomSeq
//
//    // TODO
////      newAtomSeq.head match {
////        case BinCompare(lhs, rhs, "<") if isConst(lhs) && isConst(rhs) => (lhs,rhs) match{
////          case (IntNum(l),IntNum(r)) => if l < r then Seq() else newAtomSeq
////        }
////      }
//    newAtomSeq
//  }

  // TODO add more cases (e.g. more rules) ? Preserve type of term ?
  // probably no recursive call needed here in the beginning since called in visitTerm
  protected override def normalize(term: Term): Term = {
    if !this.config.simplifyArithmetic then return term
    val typ: Type = term.typ match { // assumed that program was typechecked before and every term thus has a type
      case Some(termType: TermType) => termType.ty
      case _ => TAny // below only tested whether TInt or TDouble
    }

    val newTerm = term match {
      case BinOp(lhs, rhs, "+") => simplifyAdd(lhs, rhs, typ, term)
      case BinOp(lhs, rhs, "-") => simplifySub(lhs, rhs, typ, term)
      case BinOp(lhs, rhs, "*") => simplifyMul(lhs, rhs, typ, term)
      case BinOp(lhs, rhs, "/") => simplifyDiv(lhs, rhs, typ, term)
      
      case BinOp(lhs, rhs, "%") => (lhs, rhs) match {
        case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
        case (IntNum(l), IntNum(r)) => IntNum(l % r)
        case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l % r)
        case (l, r) => term
      }
      case BinOp(lhs, rhs, "min") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
        case (DoubleNum(l), DoubleNum(r)) => if l < r then DoubleNum(l) else DoubleNum(r)
        case (lvar, IntNum(r)) => Min(IntNum(r), lvar)
        case (lvar, DoubleNum(r)) => Min(DoubleNum(r), lvar)
        case (l, r) => term
      }
      case BinOp(lhs, rhs, "max") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
        case (DoubleNum(l), DoubleNum(r)) => if l > r then DoubleNum(l) else DoubleNum(r)
        case (l, r) if typ == TInt => Mul(IntNum(-1), normalize(Min(normalize(Mul(IntNum(-1), l)), normalize(Mul(IntNum(-1), r)))))
        case (l, r) if typ == TDouble => Mul(DoubleNum(-1), normalize(Min(normalize(Mul(DoubleNum(-1), l)), normalize(Mul(DoubleNum(-1), r)))))
        //        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
        //        case (lvar, DoubleNum(r)) => simplify(Max(DoubleNum(r), lvar))
        case (l, r) => term
      }
      case UnOp(t, "abs") => t match {
        case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
        case DoubleNum(value) => if value >= 0 then DoubleNum(value) else DoubleNum(-1 * value)
        case s => term
      }
      case BinOp(lhs, rhs, op) => BinOp(normalize(lhs), normalize(rhs), op)
      case _ => super.normalize(term)
    }
    newTerm.typ = term.typ // TODO needed?
    newTerm
  }


  // TODO compare hashCodes in the following functions or names of vars ?
  //  when hashes are used it happens that constants are propagated without the option in Config
  //  this can lead to other equalities not being found (without the option set to true) (see test "Add nested multiple times"
  private def simplifyAdd(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
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

      case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "+") // this and following four for commutativity & associativity
      case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "+")
      case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "+")) => BinOp(lNum, normalize(BinOp(lhs, rterm, "+")), "+")
      case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getIdOf(lBinOp) > getIdOf(rBinOp) then Add(rBinOp, lBinOp) else term
      case (lBinOp@BinOp(_, _, _), rhs) => normalize(BinOp(rhs, lBinOp, "+"))
      case (l, r) => term
    }

  private def simplifySub(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => normalize(Mul(rhs, IntNum(-1)))
//      case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(0) else if typ == TDouble then DoubleNum(0) else term
      case (l, r) if getIdOf(l) == getIdOf(r) => if typ == TInt then IntNum(0) else if typ == TDouble then DoubleNum(0) else term
      case (IntNum(l), IntNum(r)) => IntNum(l - r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l - r)

      case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => normalize(BinOp(IntNum(l - r), normalize(Mul(rvar, IntNum(-1))), "+"))
      case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "+")) => normalize(BinOp(DoubleNum(l - r), normalize(Mul(rvar, DoubleNum(-1))), "+"))

      case (lTerm, BinOp(IntNum(r), rTerm, "+")) => if getIdOf(lTerm) == getIdOf(rTerm) then IntNum(-r) else term
      case (lTerm, BinOp(DoubleNum(r), rTerm, "+")) => if getIdOf(lTerm) == getIdOf(rTerm) then DoubleNum(-r) else term

      case (BinOp(l,r,"+"),rhs) if getIdOf(l) == getIdOf(rhs) => r
      case (BinOp(l,r,"+"),rhs) if getIdOf(r) == getIdOf(rhs) => l

      case (l, r) => term
    }

  private def simplifyMul(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
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

    case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "*") // this and following four for commutativity & associativity
    case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "*")
    case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "*")) => normalize(BinOp(lNum, normalize(BinOp(lhs, rterm, "*")), "*"))
    case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getIdOf(lBinOp) > getIdOf(rBinOp) then Mul(rBinOp, lBinOp) else term
    case (lBinOp@BinOp(_, _, _), rhs) => normalize(BinOp(rhs, lBinOp, "*"))

    case (factor, BinOp(lhs, rhs, "+")) => distributivity(factor,lhs,rhs,Add,Mul)
    case (factor, BinOp(lhs, rhs, "-")) => distributivity(factor,lhs,rhs,Sub,Mul)

    case (lTerm, BinOp(lhs, rTerm, "/")) => if getIdOf(lTerm) == getIdOf(rTerm) then lhs else term
    case (l, r) => term
  }

  private def simplifyDiv(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
//    case (vari@Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else term
    case (l, r) if getIdOf(l) == getIdOf(r) => if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else term
    case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // int/int yields int in scala
    case (DoubleNum(l), DoubleNum(r)) if r != 0 => DoubleNum(l / r)

    case (BinOp(lhs, lTerm, "*"), rTerm) => if getIdOf(lTerm) == getIdOf(rTerm) then lhs else term
    case (lTerm, BinOp(lhs, rTerm, "*")) if getIdOf(lTerm) == getIdOf(rTerm) =>
      if typ == TInt then normalize(Div(IntNum(1), lhs)) else if typ == TDouble then normalize(Div(DoubleNum(1), lhs)) else term

    case (BinOp(lhs, rhs, "+"),denom) => distributivity(denom,lhs,rhs,Add,Div)
    case (BinOp(lhs, rhs, "-"),denom) => distributivity(denom,lhs,rhs,Sub,Div)

    case (l, r) => term
  }


  private def associativityInt(l: Int, r: Int, restTerm: Term, intOp: (Int, Int) => Int, op: (Term, Term) => BinOp): Term =
    op(IntNum(intOp(l, r)), restTerm)
  private def associativityDouble(l: Double, r: Double, restTerm: Term, doubleOp: (Double, Double) => Double, op: (Term, Term) => BinOp): Term =
    op(DoubleNum(doubleOp(l, r)), restTerm)
  private def distributivity(factor: Term, lhs: Term, rhs: Term, opOuter: (Term, Term) => BinOp, opInner: (Term, Term) => BinOp): Term =
    normalize(opOuter(normalize(opInner(lhs,factor)), normalize(opInner(rhs,factor))))


}
