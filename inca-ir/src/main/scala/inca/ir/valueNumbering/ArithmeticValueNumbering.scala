package inca.ir.valueNumbering

import inca.ir.{Atom, Name, RefByName, TAny, Term, TermType, Type, Var}
import inca.ir.extension.arithmetic.*



trait ArithmeticValueNumbering(config: ConfigVN = ConfigVN()) extends BaseValueNumbering {

  protected override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

  // in the beginning no recursive call needed here since called in visitTerm ->  all subterms visited already
  protected override def normalize(term: Term): Term = {
    if !this.config.normalize then return term
    val typ = term.typ match { // assumed that program was typechecked before and every term thus has a type
      case Some(termType: TermType) => termType
      case _ => throw new IllegalStateException(s"Untyped Term $term in normalization")
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
    val newLhs = getArgumentOfOp(lhs)
    val newRhs = getArgumentOfOp(rhs)
    return (newLhs, newRhs)
  }
  private def getArgumentOfOp(t: Term): Term = {
    if (config.useDefiningTerm) return visitTerm(getDefiningTerm(t)).head // TODO without visitTerm defterm might contain removed Var -> other solution (just replace not complete revisit?) ? update defterm here?
    else return t
  }

  private def normalizeAdd(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => rhs
      case (IntNum(l), IntNum(r)) => IntNum(l + r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l + r)

      case (IntNum(l), BinOp(IntNum(r), restTerm, "+")) => associativityInt(l,r,restTerm,_+_,Add)
      case (BinOp(IntNum(l), restTerm, "+"), IntNum(r)) => associativityInt(l,r,restTerm,_+_,Add)
      case (DoubleNum(l), BinOp(DoubleNum(r), restTerm, "+")) => associativityDouble(l,r,restTerm,_+_,Add)
      case (BinOp(DoubleNum(l), restTerm, "+"), DoubleNum(r)) => associativityDouble(l,r,restTerm,_+_,Add)

      case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ.ty == TInt => normalize(Mul(IntNum(2), lhs).typed(typ)) // x + x == 2*x for Ints
      case (lhs, BinOp(IntNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(IntNum(x + 1), lhs).typed(typ))
      case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ.ty == TDouble => normalize(Mul(DoubleNum(2), lhs).typed(typ)) // for Doubles
      case (lhs, BinOp(DoubleNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(DoubleNum(x + 1), lhs).typed(typ))

      // for commutativity & associativity
//      case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => if nameL <= nameR then Add(lvar,rvar) else Add(rvar, lvar) // variables
//      case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => Add(rNum, lvar) // numbers before vars
//
//      case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "+")) => Add(lNum, normalize(Add(lhs, rterm).typed(typ)))
//      case (lhs, BinOp(lVar@Var(_), rterm, "+")) if !isConst(lhs) => normalize(Add(lVar, normalize(Add(lhs, rterm).typed(typ))).typed(typ)) // outer recursion necessary for ordering correctly but leads to stack overflow in other test
//
//      case (lvar@Var(RefByName(Name(nameL))), BinOp(rvar@Var(RefByName(Name(nameR))), rterm, "+")) if nameL > nameR =>
//        Add(rvar, normalize(Add(lvar, rterm).typed(typ)))
//      case (lBinOp@BinOp(ll, lr, "+"), rBinOp@BinOp(rl, rr, "+")) => normalize(Add(ll, normalize(Add(lr,rBinOp).typed(typ))).typed(typ))
//      case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) if getIdOf(lBinOp) > getIdOf(rBinOp) => normalize(Add(rBinOp, lBinOp).typed(typ))
//      case (lBinOp@BinOp(_, _, _), rhs) if !rhs.isInstanceOf[BinOp] => normalize(Add(rhs, lBinOp).typed(typ))

      case (l, r) => orderAssociativityCommutativity(getAllOperands(Add(l,r),"+"), typ, "+")
    }

  private def normalizeSub(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match { // fold or rewrite to Add(lhs, Mul(-1, rhs))
      case (_, IntNum(0)) | (_, DoubleNum(0)) => lhs
      case (IntNum(0), _) | (DoubleNum(0), _) => normalize(Mul(rhs, IntNum(-1)).typed(typ))
      case (l, r) if getIdOf(l) == getIdOf(r) =>
        if typ.ty == TInt then IntNum(0)
        else if typ.ty == TDouble then DoubleNum(0)
        else throw new IllegalStateException(s"Sub with $lhs and $rhs with unknown type $typ normalization")
      case (IntNum(l), IntNum(r)) => IntNum(l - r)
      case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l - r)

      case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => normalize(Add(IntNum(l - r), normalize(Mul(rvar, IntNum(-1)).typed(typ))).typed(typ))
      case (DoubleNum(l), BinOp(DoubleNum(r), rvar, "+")) => normalize(Add(DoubleNum(l - r), normalize(Mul(rvar, DoubleNum(-1)).typed(typ))).typed(typ))

      case (lTerm, rBinOp@BinOp(IntNum(r), rTerm, "+")) if getIdOf(lTerm) == getIdOf(rTerm) => IntNum(-r)
      case (lTerm, rBinOp@BinOp(DoubleNum(r), rTerm, "+")) if getIdOf(lTerm) == getIdOf(rTerm)  => DoubleNum(-r)

      case (BinOp(l,r,"+"),rhs) if getIdOf(l) == getIdOf(rhs) => r
      case (BinOp(l,r,"+"),rhs) if getIdOf(r) == getIdOf(rhs) => l

      case (l, r) =>
        if typ.ty == TInt then Add(l,Mul(IntNum(-1),r).typed(typ))
        else if typ.ty == TDouble then Add(l,Mul(DoubleNum(-1),r).typed(typ))
        else throw new IllegalStateException(s"Sub with $lhs and $rhs with unknown type $typ normalization")
    }

  private def normalizeMul(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
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

//    case (lvar@Var(RefByName(Name(nameL))), rvar@Var(RefByName(Name(nameR)))) => // this and following five for commutativity & associativity
//      if nameL <= nameR then Mul(lvar,rvar)else Mul(rvar, lvar)
//    case (lvar@Var(RefByName(Name(nameL))), rNum@(IntNum(_) | DoubleNum(_))) => Mul(rNum, lvar)
//    case (lhs, BinOp(lNum@(IntNum(_) | DoubleNum(_)), rterm, "*")) =>
//      normalize(Mul(lNum, normalize(Mul(lhs, rterm).typed(typ))).typed(typ))
//    case (lvar@Var(RefByName(Name(nameL))), BinOp(rvar@Var(RefByName(Name(nameR))), rterm, "*")) if nameL > nameR =>
//      Mul(rvar, normalize(Mul(lvar, rterm).typed(typ))).typed(typ)
//    case (lBinOp@BinOp(_, _, _), rBinOp@BinOp(_, _, _)) if getIdOf(lBinOp) > getIdOf(rBinOp) => normalize(Mul(rBinOp, lBinOp).typed(typ))
//    case (lBinOp@BinOp(_, _, _), rhs) if !rhs.isInstanceOf[BinOp] => normalize(Mul(rhs, lBinOp).typed(typ))

    case (factor, BinOp(lhs, rhs, "+")) => distributivity(factor,lhs,rhs,Add,Mul,typ)
//    case (factor, BinOp(lhs, rhs, "-")) => distributivity(factor,lhs,rhs,Sub,Mul,typ)

    case (lTerm, BinOp(lhs, rBinOp@rTerm, "/")) => // TODO include ?
      if getIdOf(lTerm) == getIdOf(rTerm) then lhs else Mul(lTerm, rBinOp)

    case (l, r) => orderAssociativityCommutativity(getAllOperands(Mul(l,r),"*"), typ, "*")
  }

  private def normalizeDiv(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
//    case (vari@Var(RefByName(Name(l))), Var(RefByName(Name(r)))) if l == r => if typ == TInt then IntNum(1) else if typ == TDouble then DoubleNum(1) else term
    case (l, r) if (getIdOf(l) == getIdOf(r) && getReplacementTerm(r) != IntNum(0) && getReplacementTerm(r) != DoubleNum(0)) =>
      if typ.ty == TInt then IntNum(1) else if typ.ty == TDouble then DoubleNum(1) else Div(l,r) // TODO 0/0 -> 1
    case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r) // int/int yields int in scala
    case (DoubleNum(l), DoubleNum(r)) if r != 0 => DoubleNum(l / r)

    case (lBinOp@BinOp(lhs, lTerm, "*"), rTerm) => if getIdOf(lTerm) == getIdOf(rTerm) then lhs else Div(lBinOp,rTerm)
    case (lTerm, lBinOp@BinOp(lhs, rTerm, "*")) if getIdOf(lTerm) == getIdOf(rTerm) =>
      if typ.ty == TInt then normalize(Div(IntNum(1), lhs).typed(typ))
      else if typ.ty == TDouble then normalize(Div(DoubleNum(1), lhs).typed(typ))
      else Div(lTerm,lBinOp)

    case (BinOp(lhs, rhs, "+"),denom) => distributivity(denom,lhs,rhs,Add,Div,typ)
//    case (BinOp(lhs, rhs, "-"),denom) => distributivity(denom,lhs,rhs,Sub,Div,typ)

    case (l, r) => Div(l,r)
  }

  private def normalizeRemainder(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
    case (IntNum(0),_) => IntNum(0)
    case (DoubleNum(0),_) => DoubleNum(0)
    case (IntNum(l), IntNum(r)) => IntNum(l % r)
    case (DoubleNum(l), DoubleNum(r)) => DoubleNum(l % r)
    case (lhs,rhs) if getIdOf(lhs) == getIdOf(rhs) => if typ.ty == TInt then IntNum(0) else DoubleNum(0)
    case (BinOp(l,r,"*"),rhs) if getIdOf(l) == getIdOf(rhs) => if typ.ty == TInt then IntNum(0) else DoubleNum(0)
    case (BinOp(l,r,"*"),rhs) if getIdOf(r) == getIdOf(rhs) => if typ.ty == TInt then IntNum(0) else DoubleNum(0)
    case (l, r) => Remainder(l,r)
  }

  private def normalizeMin(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l < r then DoubleNum(l) else DoubleNum(r)
    case (lvar, IntNum(r)) => Min(IntNum(r), lvar)
    case (lvar, DoubleNum(r)) => Min(DoubleNum(r), lvar)
    case (l, r) => Min(l,r)
  }

  private def normalizeMax(lhs: Term, rhs: Term, typ: TermType): Term =  getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l > r then DoubleNum(l) else DoubleNum(r)
    case (l, r) if typ.ty == TInt =>
      Mul(IntNum(-1), normalize(Min(normalize(Mul(IntNum(-1), l).typed(typ)), normalize(Mul(IntNum(-1), r).typed(typ))).typed(typ)))
    case (l, r) if typ.ty == TDouble =>
      Mul(DoubleNum(-1), normalize(Min(normalize(Mul(DoubleNum(-1), l).typed(typ)), normalize(Mul(DoubleNum(-1), r).typed(typ))).typed(typ)))
    //        case (lvar, IntNum(r)) => simplify(Max(IntNum(r), lvar))
    //        case (lvar, DoubleNum(r)) => simplify(Max(DoubleNum(r), lvar))
    case (l, r) => throw new IllegalStateException(s"unknown type $typ in normalization of Max with $lhs and $rhs")
  }

  private def normalizeAbs(t: Term, typ: TermType): Term = getArgumentOfOp(t) match {
    case IntNum(value) => if value >= 0 then IntNum(value) else IntNum(-1 * value)
    case DoubleNum(value) => if value >= 0 then DoubleNum(value) else DoubleNum(-1 * value)
    case arg => Abs(arg)
  }

  private def associativityInt(l: Int, r: Int, restTerm: Term, intOp: (Int, Int) => Int, op: (Term, Term) => BinOp): Term =
    op(IntNum(intOp(l, r)), restTerm)
  private def associativityDouble(l: Double, r: Double, restTerm: Term, doubleOp: (Double, Double) => Double, op: (Term, Term) => BinOp): Term =
    op(DoubleNum(doubleOp(l, r)), restTerm)
  private def distributivity(factor: Term, lhs: Term, rhs: Term, opOuter: (Term, Term) => BinOp, opInner: (Term, Term) => BinOp, typ: TermType): Term = {
    val innerL = opInner(lhs, factor).typed(typ)
    val innerR = opInner(rhs, factor).typed(typ)
    val res = opOuter(normalize(innerL), normalize(innerR)).typed(typ)
    normalize(res)
  }

  // methods for associativity and commutativity (kind of op (Add, Mul) passed as argument)

  private def getAllOperands(term: Term, op: String): Seq[Term] = term match {
    case BinOp(lhs, rhs, opStr) if opStr == op => getAllOperands(lhs, op) ++ getAllOperands(rhs, op)
    case _ => Seq(term)
  }

  private def sortedByID(terms: Seq[Term]): Seq[Term] = terms.sortWith { case (l, r) => getIdOf(l) < getIdOf(r) }

  // result should in general be of form (num + (var1 + (var2 + ... + (num * Var + (...))))
  private def orderAssociativityCommutativity(operands: Seq[Term], typ: TermType, op: String): Term = {
    val neutralElem = op match {
      case "+" => 0
      case "*" => 1
    }
    val number = (typ.ty, op) match {
      case (TInt, "+") => IntNum(operands.filter(_.isInstanceOf[IntNum]).foldRight(neutralElem) { case (IntNum(n1), n2) => n1 + n2 })
      case (TDouble, "+") => DoubleNum(operands.filter(_.isInstanceOf[DoubleNum]).foldRight(neutralElem.toDouble) { case (DoubleNum(n1), n2) => n1 + n2 })
      case (TInt, "*") => IntNum(operands.filter(_.isInstanceOf[IntNum]).foldRight(neutralElem) { case (IntNum(n1), n2) => n1 * n2 })
      case (TDouble, "*") => DoubleNum(operands.filter(_.isInstanceOf[DoubleNum]).foldRight(neutralElem.toDouble) { case (DoubleNum(n1), n2) => n1 * n2 })
    }
    val vars = operands.filter(_.isInstanceOf[Var]).sortWith { case (v1@Var(_), v2@Var(_)) => v1.name.name < v2.name.name }
    val unOps = operands.filter(_.isInstanceOf[UnOp]) // just one UnOp: Abs
    val muls = if (op != "*") operands.filter {
      case BinOp(_, _, "*") => true
      case _ => false
    } else Seq()
    val divs = operands.filter {
      case BinOp(_, _, "/") => true
      case _ => false
    }
    val remains = operands.filter {
      case BinOp(_, _, "%") => true
      case _ => false
    }
    val mins = operands.filter {
      case BinOp(_, _, "min") => true
      case _ => false
    }

    val newOperands = (if number != IntNum(neutralElem) && number != DoubleNum(neutralElem) then Seq(number) else Seq())
      ++ vars ++ Seq(unOps, muls, divs, remains, mins).flatMap(sortedByID)
    buildOp(newOperands, typ, (l, r) => BinOp(l, r, op))
  }

  private def buildOp(operands: Seq[Term], typ: TermType, binOp: (Term, Term) => BinOp): Term = operands match {
    case Nil => throw new IllegalStateException(s"no terms in Operation")
    case head :: Nil => head
    case h1 :: h2 :: tail => binOp(h1, buildOp(h2 :: tail, typ, binOp)).typed(typ)
  }

}
