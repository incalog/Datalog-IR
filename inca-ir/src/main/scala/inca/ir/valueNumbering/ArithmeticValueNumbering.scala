package inca.ir.valueNumbering

import inca.ir.{Name, RefByName, Term, Var}
import inca.ir.extension.arithmetic.*

trait ArithmeticValueNumbering(config: ConfigVN) extends BaseValueNumbering {

  override def visitTerm(term: Term): Seq[Term] = term match{
    case IntNum(_) | DoubleNum(_) => Seq(term)    // otherwise occurrences of such trivial exps are also replaced by a Var
    case _ => super.visitTerm(term)
  }

  // TODO add more cases (e.g. more rules, DoubleNum)
  //  probably no recursive call needed here since called in visitTerm
  protected def simplify(term: Term): Term = {
    if !this.config.simplifyArithmetic then return term
    term match {
      case DoubleNum(value) if value.isWhole => IntNum(value.intValue())
      case BinOp(lhs, rhs, "+") => (lhs, rhs) match {
        case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
        case (IntNum(0), _) | (DoubleNum(0), _) => rhs
        case (IntNum(l), IntNum(r)) => IntNum(l + r)
        case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => BinOp(IntNum(l + r), rvar, "+")
        case (IntNum(l), BinOp(rvar, IntNum(r), "+")) => BinOp(IntNum(l + r), rvar, "+")
        case (BinOp(IntNum(l), lvar, "+"), IntNum(r)) => BinOp(IntNum(l + r), lvar, "+")
        case (BinOp(lvar, IntNum(l), "+"), IntNum(r)) => BinOp(IntNum(l + r), lvar, "+")
        case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "+") // this and following two for commutativity
        case (lvar@Var(RefByName(Name(nameL))), rInt@IntNum(_)) => BinOp(rInt, lvar, "+")
        case (lBinOp@BinOp(_, _, _), rhs) => BinOp(rhs, lBinOp, "+")
        case (l, r) => BinOp(l, r, "+")
      }
      case BinOp(lhs, rhs, "-") => (lhs, rhs) match {
        case (l, IntNum(0)) => l
        case (IntNum(0), r) => r
        case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(0) else BinOp(Var(Name(l)), Var(Name(r)), "-")
        case (IntNum(l), IntNum(r)) => IntNum(l - r)
        case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => BinOp(IntNum(l - r), Mul(rvar, IntNum(-1)), "+")
        case (IntNum(l), BinOp(lvar, IntNum(r), "+")) => BinOp(IntNum(l - r), Mul(lvar, IntNum(-1)), "+")
        case (Var(RefByName(Name(nameL))), BinOp(IntNum(r), Var(RefByName(Name(nameR))), "+")) => if nameL == nameR then IntNum(-r) else term
        case (l, r) => BinOp(l, r, "-")
      }
      case BinOp(lhs, rhs, "*") => (lhs, rhs) match {
        case (_, IntNum(0)) | (IntNum(0), _) => IntNum(0)
        case (IntNum(1), r) => r
        case (l, IntNum(1)) => l
        case (IntNum(l), IntNum(r)) => IntNum(l * r)
        case (IntNum(l), BinOp(IntNum(r), rvar, "*")) => BinOp(IntNum(l * r), rvar, "*")
        case (IntNum(l), BinOp(rvar, IntNum(r), "*")) => BinOp(IntNum(l * r), rvar, "*")
        case (BinOp(IntNum(l), lvar, "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
        case (BinOp(lvar, IntNum(l), "*"), IntNum(r)) => BinOp(IntNum(l * r), lvar, "*")
        case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then term else BinOp(rvar, lvar, "*") // this and following two for commutativity
        case (lvar@Var(RefByName(Name(nameL))), rInt@IntNum(_)) => BinOp(rInt, lvar, "*")
        case (lBinOp@BinOp(_, _, _), rhs) => BinOp(rhs, lBinOp, "*")
        //        case (lInt@IntNum(l), BinOp(IntNum(r),rvar,"+")) => Add(IntNum(l*r),Mul(lInt,rvar)) //  for distributivity
        case (l, BinOp(lhs, rhs, "+")) => Add(simplify(Mul(l, lhs)), simplify(Mul(l, rhs))) //  for distributivity
        case (l, r) => BinOp(l, r, "*")
      }
      case BinOp(lhs, rhs, "/") => (lhs, rhs) match {
        case (l, IntNum(1)) => l
        case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(1) else term
        case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // TODO int/int yields int in scala
        case (l, r) => BinOp(l, r, "/")
      }
      case BinOp(lhs, rhs, "%") => (lhs, rhs) match {
        case (l, IntNum(1)) => l
        case (IntNum(l), IntNum(r)) => IntNum(l % r)
        case (l, r) => BinOp(l, r, "%")
      }
      case BinOp(lhs, rhs, "min") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
        case (lvar, IntNum(r)) => Min(IntNum(r), lvar)
        case (l, r) => BinOp(l, r, "min")
      }
      case BinOp(lhs, rhs, "max") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
        case (l, r) => Mul(IntNum(-1), Min(simplify(Mul(IntNum(-1), l)), simplify(Mul(IntNum(-1), r))))
      }
      case UnOp(t, "abs") => t match {
        case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
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
