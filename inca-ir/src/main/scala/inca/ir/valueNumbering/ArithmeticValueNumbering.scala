package inca.ir.valueNumbering

import inca.ir.{Name, RefByName, Term, Var}
import inca.ir.extension.arithmetic.*

trait ArithmeticValueNumbering(config: ConfigVN) extends BaseValueNumbering {

  override def visitTerm(term: Term): Seq[Term] = term match{
    case IntNum(_) | DoubleNum(_) => Seq(term)    // otherwise occurrences of such trivial exps are also replaced by a Var
    case _ => super.visitTerm(term)
  }

  // TODO add more cases (e.g. more rules) ?
  // probably no recursive call needed here in the beginning since called in visitTerm
  protected def simplify(term: Term): Term = {
    if !this.config.simplifyArithmetic then return term
    term match {
//      case DoubleNum(value) if value.isWhole => IntNum(value.intValue())
      case BinOp(lhs, rhs, "+") => (lhs, rhs) match {
        case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
        case (IntNum(0), _) | (DoubleNum(0), _) => rhs
        case (IntNum(l), IntNum(r)) => IntNum(l + r)
        case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l + r)
//        case (DoubleNum(l), IntNum(r)) => DoubleNum(l + r)
//        case (IntNum(l), DoubleNum(r)) => DoubleNum(l + r)
        case (IntNum(l), BinOp(IntNum(r), rterm, "+")) => BinOp(IntNum(l + r), rterm, "+")
        case (IntNum(l), BinOp(rterm, IntNum(r), "+")) => BinOp(IntNum(l + r), rterm, "+")
        case (BinOp(IntNum(l), rterm, "+"), IntNum(r)) => BinOp(IntNum(l + r), rterm, "+")
        case (BinOp(lterm, IntNum(l), "+"), IntNum(r)) => BinOp(IntNum(l + r), lterm, "+")
        case (DoubleNum(l), BinOp(DoubleNum(r), rterm, "+")) => BinOp(DoubleNum(l + r), rterm, "+")
        case (DoubleNum(l), BinOp(rterm, DoubleNum(r), "+")) => BinOp(DoubleNum(l + r), rterm, "+")
        case (BinOp(DoubleNum(l), lterm, "+"), DoubleNum(r)) => BinOp(DoubleNum(l + r), lterm, "+")
        case (BinOp(lterm, DoubleNum(l), "+"), DoubleNum(r)) => BinOp(DoubleNum(l + r), lterm, "+")
//        case (BinOp(lhs, rhs, "+"),rrhs) =>
//          val temp = simplify(Add(rrhs, lhs))
//          if (temp != Add(rrhs, lhs)) {
//            Add(temp, rhs)
//          } else {
//            Add(simplify(Add(rrhs, rhs)), lhs)
//          }
//        case (llhs, BinOp(lhs, rhs, "+")) =>
//          val temp = simplify(Add(llhs,lhs))
//          if (temp != Add(llhs,lhs)) {
//            Add(temp,rhs)
//          } else {
//            Add(simplify(Add(llhs,rhs)),lhs)
//          }
        case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "+") // this and following four for commutativity & associativity
        case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_)|DoubleNum(_))) => BinOp(rNum, lvar, "+")
        case (lhs, BinOp(lNum@(IntNum(_)|DoubleNum(_)), rterm, "+")) => BinOp(lNum, simplify(BinOp(lhs, rterm, "+")), "+")
        //        case (lhs, BinOp(lterm, rNum@(IntNum(_) | DoubleNum(_)),  "+")) => BinOp(rNum, simplify(BinOp(lhs, lterm, "+")), "+")
        case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getHashCode(lBinOp) > getHashCode(rBinOp) then Add(rBinOp,lBinOp) else term
        case (lBinOp@BinOp(_, _, _), rhs) => simplify(BinOp(rhs, lBinOp, "+"))
        case (l, r) => BinOp(l, r, "+")
      }
      case BinOp(lhs, rhs, "-") => (lhs, rhs) match {
        case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
        case (IntNum(0), _) | (DoubleNum(0), _) => simplify(Mul(rhs,IntNum(-1)))
        case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(0) else BinOp(Var(Name(l)), Var(Name(r)), "-")
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
        case (l, r) => BinOp(l, r, "-")
      }
      case BinOp(lhs, rhs, "*") => (lhs, rhs) match {
        case (_, IntNum(0)) | (IntNum(0), _) => IntNum(0)
        case (_, DoubleNum(0)) | (DoubleNum(0), _) => DoubleNum(0)
        case (IntNum(1), _) | (DoubleNum(1), _) => rhs
        case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
        case (IntNum(l), IntNum(r)) => IntNum(l * r)
        case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l * r)
        case (IntNum(l), BinOp(IntNum(r), rvar, "*")) => BinOp(IntNum(l * r), rvar, "*")  // for associativity
        case (IntNum(l), BinOp(rvar, IntNum(r), "*")) => BinOp(IntNum(l * r), rvar, "*")
        case (BinOp(IntNum(l), lvar, "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
        case (BinOp(lvar, IntNum(l), "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
        case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "*")) => BinOp(DoubleNum(l * r), rvar, "*")
        case (DoubleNum(l), BinOp(rvar, DoubleNum(r), "*")) => BinOp(DoubleNum(l * r), rvar, "*")
        case (BinOp(DoubleNum(l), lvar, "*"), DoubleNum(r)) => BinOp(DoubleNum(l * r), lvar, "*")
        case (BinOp(lvar, DoubleNum(l), "*"), DoubleNum(r)) => BinOp(DoubleNum(l * r), lvar, "*")
        case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "*") // this and following four for commutativity & associativity
        case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => BinOp(rNum, lvar, "*")
        case (lhs, BinOp(lNum@(IntNum(_)|DoubleNum(_)), rterm, "*")) => BinOp(lNum, simplify(BinOp(lhs, rterm, "*")), "*")
        case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) => if getHashCode(lBinOp) > getHashCode(rBinOp) then Mul(rBinOp,lBinOp) else term
        case (lBinOp@BinOp(_, _, _), rhs) => simplify(BinOp(rhs, lBinOp, "*"))
        case (l, BinOp(lhs, rhs, "+")) => Add(simplify(Mul(l, lhs)), simplify(Mul(l, rhs))) //  for distributivity
        case (Var(RefByName(Name(nameL))), BinOp(lhs, Var(RefByName(Name(nameR))), "/")) => if nameL == nameR then lhs else term
//        case (Var(RefByName(Name(nameL))), BinOp(DoubleNum(l), Var(RefByName(Name(nameR))), "/")) => if nameL == nameR then DoubleNum(l) else term
        case (l, r) => BinOp(l, r, "*")
      }
      case BinOp(lhs, rhs, "/") => (lhs, rhs) match {
        case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
        case (vari@Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(1) else term // TODO doubles
        case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // TODO int/int yields int in scala
        case (DoubleNum(l), DoubleNum(r)) if r != 0 => DoubleNum(l / r)
        case (BinOp(lhs, Var(RefByName(Name(nameL))), "*"), Var(RefByName(Name(nameR)))) => if nameL == nameR then lhs else term
        case (Var(RefByName(Name(nameL))), BinOp(lhs, Var(RefByName(Name(nameR))), "*")) => if nameL == nameR then simplify(Div(IntNum(1),lhs)) else term // TODO doubles
        case (l, r) => BinOp(l, r, "/")
      }
      case BinOp(lhs, rhs, "%") => (lhs, rhs) match {
        case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
        case (IntNum(l), IntNum(r)) => IntNum(l % r)
        case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l % r)
        case (l, r) => BinOp(l, r, "%")
      }
      case BinOp(lhs, rhs, "min") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
        case (DoubleNum(l), DoubleNum(r)) => if l < r then DoubleNum(l) else DoubleNum(r)
        case (lvar, IntNum(r)) => Min(IntNum(r), lvar)
        case (lvar, DoubleNum(r)) => Min(DoubleNum(r), lvar)
        case (l, r) => BinOp(l, r, "min")
      }
      case BinOp(lhs, rhs, "max") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
        case (DoubleNum(l), DoubleNum(r)) => if l > r then DoubleNum(l) else DoubleNum(r)
        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
        case (lvar, DoubleNum(r)) => simplify(Max(DoubleNum(r), lvar))
        case (l, r) => Mul(IntNum(-1), Min(simplify(Mul(IntNum(-1), l)), simplify(Mul(IntNum(-1), r))))
      }
      case UnOp(t, "abs") => t match {
        case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
        case DoubleNum(value) => if value >= 0 then DoubleNum(value) else DoubleNum(-1 * value)
        case s => UnOp(s, "abs")
      }
      case BinOp(lhs, rhs, op) => BinOp(simplify(lhs), simplify(rhs), op)
      case _ => term
    }
  }

  override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

}
