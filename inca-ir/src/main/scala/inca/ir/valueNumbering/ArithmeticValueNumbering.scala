package inca.ir.valueNumbering

import inca.ir.{Atom, Name, RefByName, TAny, Term, TermType, Type, Var}
import inca.ir.extension.arithmetic.*
import inca.ir.typing.Mode.Bound

trait ArithmeticValueNumbering(config: ConfigVN) extends BaseValueNumbering {

  override def visitTerm(term: Term): Seq[Term] = term match { // TODO could use isConst(term)
    case IntNum(_) | DoubleNum(_) => Seq(term) // otherwise occurrences of such trivial exps are also replaced by a Var
    case _ => super.visitTerm(term)
  }

  //  override def visitAtom(atom: Atom): Seq[Atom] = {
  //    val newAtom = super.visitAtom(atom)
  //    if newAtom.isEmpty then return newAtom
  //    
  //    newAtom.head match {
  //      case BinCompare(lhs, rhs, "<") => 
  //    }
  //  }

  // TODO add more cases (e.g. more rules) ? Preserve type of term ?
  // probably no recursive call needed here in the beginning since called in visitTerm
  protected def simplify(term: Term): Term = {
    if !this.config.simplifyArithmetic then return term
    val typ: Type = term.typ match {
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
        case (l, r) if typ == TInt => Mul(IntNum(-1), simplify(Min(simplify(Mul(IntNum(-1), l)), simplify(Mul(IntNum(-1), r)))))
        case (l, r) if typ == TDouble => Mul(DoubleNum(-1), simplify(Min(simplify(Mul(DoubleNum(-1), l)), simplify(Mul(DoubleNum(-1), r)))))
        //        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
        //        case (lvar, DoubleNum(r)) => simplify(Max(DoubleNum(r), lvar))
        case (l, r) => term
      }
      case UnOp(t, "abs") => t match {
        case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
        case DoubleNum(value) => if value >= 0 then DoubleNum(value) else DoubleNum(-1 * value)
        case s => term
      }
      case BinOp(lhs, rhs, op) => BinOp(simplify(lhs), simplify(rhs), op)
      case _ => term
    }
    newTerm.typ = term.typ // TODO needed?
    newTerm
  }

  private def simplifyAdd(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => rhs
      case (IntNum(l), IntNum(r)) => IntNum(l + r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l + r)
      case (IntNum(l), BinOp(IntNum(r), rterm, "+")) => BinOp(IntNum(l + r), rterm, "+")
      case (IntNum(l), BinOp(rterm, IntNum(r), "+")) => BinOp(IntNum(l + r), rterm, "+")
      case (BinOp(IntNum(l), rterm, "+"), IntNum(r)) => BinOp(IntNum(l + r), rterm, "+")
      case (BinOp(lterm, IntNum(l), "+"), IntNum(r)) => BinOp(IntNum(l + r), lterm, "+")
      case (DoubleNum(l), BinOp(DoubleNum(r), rterm, "+")) => BinOp(DoubleNum(l + r), rterm, "+")
      case (DoubleNum(l), BinOp(rterm, DoubleNum(r), "+")) => BinOp(DoubleNum(l + r), rterm, "+")
      case (BinOp(DoubleNum(l), lterm, "+"), DoubleNum(r)) => BinOp(DoubleNum(l + r), lterm, "+")
      case (BinOp(lterm, DoubleNum(l), "+"), DoubleNum(r)) => BinOp(DoubleNum(l + r), lterm, "+")
      case (lhs: Term, rhs: Term) if lhs == rhs && typ == TInt => Mul(IntNum(2), lhs) // x + x == 2*x for Ints
      case (lhs, BinOp(IntNum(x), rhs, "*")) if lhs == rhs => Mul(IntNum(x + 1), lhs)
      case (lhs: Term, rhs: Term) if lhs == rhs && typ == TDouble => Mul(DoubleNum(2), lhs) // for Doubles
      case (lhs, BinOp(DoubleNum(x), rhs, "*")) if lhs == rhs => Mul(DoubleNum(x + 1), lhs)
      case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "+") // this and following four for commutativity & associativity
      case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "+")
      case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "+")) => BinOp(lNum, simplify(BinOp(lhs, rterm, "+")), "+")
      //        case (lhs, BinOp(lterm, rNum@(IntNum(_) | DoubleNum(_)),  "+")) => BinOp(rNum, simplify(BinOp(lhs, lterm, "+")), "+")
      case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getHashCode(lBinOp) > getHashCode(rBinOp) then Add(rBinOp, lBinOp) else term
      case (lBinOp@BinOp(_, _, _), rhs) => simplify(BinOp(rhs, lBinOp, "+"))
      case (l, r) => term
    }

  private def simplifySub(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => simplify(Mul(rhs, IntNum(-1)))
      case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(0) else if typ == TDouble then DoubleNum(0) else term
      case (IntNum(l), IntNum(r)) => IntNum(l - r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l - r)
      //        case (DoubleNum(l), IntNum(r)) => DoubleNum(l - r)
      //        case (IntNum(l), DoubleNum(r)) => DoubleNum(l - r)
      case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => BinOp(IntNum(l - r), simplify(Mul(rvar, IntNum(-1))), "+")
      case (IntNum(l), BinOp(lvar, IntNum(r), "+")) => BinOp(IntNum(l - r), simplify(Mul(lvar, IntNum(-1))), "+")
      case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "+")) => BinOp(DoubleNum(l - r), simplify(Mul(rvar, DoubleNum(-1))), "+")
      case (DoubleNum(l), BinOp(lvar, DoubleNum(r), "+")) => BinOp(DoubleNum(l - r), simplify(Mul(lvar, DoubleNum(-1))), "+")
      case (Var(RefByName(Name(nameL))), BinOp(IntNum(r), Var(RefByName(Name(nameR))), "+")) => if nameL == nameR then IntNum(-r) else term
      case (Var(RefByName(Name(nameL))), BinOp(DoubleNum(r), Var(RefByName(Name(nameR))), "+")) => if nameL == nameR then DoubleNum(-r) else term
      case (l, r) => term
    }

  private def simplifyMul(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
    case (_, IntNum(0)) | (IntNum(0), _) => IntNum(0)
    case (_, DoubleNum(0)) | (DoubleNum(0), _) => DoubleNum(0)
    case (IntNum(1), _) | (DoubleNum(1), _) => rhs
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
    case (IntNum(l), IntNum(r)) => IntNum(l * r)
    case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l * r)
    case (IntNum(l), BinOp(IntNum(r), rvar, "*")) => BinOp(IntNum(l * r), rvar, "*") // for associativity
    case (IntNum(l), BinOp(rvar, IntNum(r), "*")) => BinOp(IntNum(l * r), rvar, "*")
    case (BinOp(IntNum(l), lvar, "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
    case (BinOp(lvar, IntNum(l), "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
    case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "*")) => BinOp(DoubleNum(l * r), rvar, "*")
    case (DoubleNum(l), BinOp(rvar, DoubleNum(r), "*")) => BinOp(DoubleNum(l * r), rvar, "*")
    case (BinOp(DoubleNum(l), lvar, "*"), DoubleNum(r)) => BinOp(DoubleNum(l * r), lvar, "*")
    case (BinOp(lvar, DoubleNum(l), "*"), DoubleNum(r)) => BinOp(DoubleNum(l * r), lvar, "*")
    case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "*") // this and following four for commutativity & associativity
    case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "*")
    case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "*")) => BinOp(lNum, simplify(BinOp(lhs, rterm, "*")), "*")
    case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getHashCode(lBinOp) > getHashCode(rBinOp) then Mul(rBinOp, lBinOp) else term
    case (lBinOp@BinOp(_, _, _), rhs) => simplify(BinOp(rhs, lBinOp, "*"))
    case (l, BinOp(lhs, rhs, "+")) => Add(simplify(Mul(l, lhs)), simplify(Mul(l, rhs))) //  for distributivity
    case (Var(RefByName(Name(nameL))), BinOp(lhs, Var(RefByName(Name(nameR))), "/")) => if nameL == nameR then lhs else term
    //        case (Var(RefByName(Name(nameL))), BinOp(DoubleNum(l), Var(RefByName(Name(nameR))), "/")) => if nameL == nameR then DoubleNum(l) else term
    case (l, r) => term
  }

  private def simplifyDiv(lhs: Term, rhs: Term, typ: Type, term: Term): Term = (lhs, rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
    case (vari@Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else term
    case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // int/int yields int in scala
    case (DoubleNum(l), DoubleNum(r)) if r != 0 => DoubleNum(l / r)
    case (BinOp(lhs, Var(RefByName(Name(nameL))), "*"), Var(RefByName(Name(nameR)))) => if nameL == nameR then lhs else term
    case (Var(RefByName(Name(nameL))), BinOp(lhs, Var(RefByName(Name(nameR))), "*")) if nameL == nameR =>
      if typ == TInt then simplify(Div(IntNum(1), lhs)) else if typ == TDouble then simplify(Div(DoubleNum(1), lhs)) else term
    case (l, r) => term
  }
  

  override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

}
