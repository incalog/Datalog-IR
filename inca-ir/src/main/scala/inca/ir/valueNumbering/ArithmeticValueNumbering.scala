package inca.ir.valueNumbering

import inca.ir.{Term, TermType, Var}
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

  private def normalizeAdd(lhs: Term, rhs: Term, typ: TermType, repetition: Boolean = false): Term = getArgumentsOfOp(lhs,rhs) match {
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

      case (l, r) => if !repetition then orderAssociativityCommutativity(getAllOperands(Add(l,r),"+"), typ, "+") else Add(l,r)
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
        if typ.ty == TInt then normalize(Add(l, normalize(Mul(IntNum(-1),r).typed(typ)) ).typed(typ))
        else if typ.ty == TDouble then normalize(Add(l, normalize(Mul(DoubleNum(-1),r).typed(typ)) ).typed(typ))
        else throw new IllegalStateException(s"Sub with $lhs and $rhs with unknown type $typ normalization")
    }

  private def normalizeMul(lhs: Term, rhs: Term, typ: TermType, repetition: Boolean = false): Term = getArgumentsOfOp(lhs,rhs) match {
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

    case (factor, BinOp(lhs, rhs, "+")) => distributivity(factor,lhs,rhs,Add,Mul,typ)

    case (lTerm, BinOp(lhs, rBinOp@rTerm, "/")) => // TODO include ?
      if getIdOf(lTerm) == getIdOf(rTerm) then lhs else Mul(lTerm, rBinOp)

    case (l, r) => if !repetition then orderAssociativityCommutativity(getAllOperands(Mul(l,r),"*"), typ, "*") else Mul(l,r)
  }

  private def normalizeDiv(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(1)) | (_, DoubleNum(1)) => lhs
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
    buildOp(newOperands, typ, op)
  }

  private def buildOp(operands: Seq[Term], typ: TermType, op: String): Term = {
    val binOp = (l, r) => BinOp(l, r, op)
    val normOp = op match
      case "+" => normalizeAdd(_, _, typ, repetition = true)
      case "*" => normalizeMul(_, _, typ, repetition = true)

    def buildOpInner(operands: Seq[Term]): Term = operands match {
      case Nil => throw new IllegalStateException(s"no terms in Operation")
      case head :: Nil => head
      case h1 :: h2 :: tail =>
        val recRes = buildOpInner(h2 :: tail).typed(typ, force = true)
        val newRes = binOp(h1, normalize(recRes)).typed(typ)
        normOp(newRes.lhs, newRes.rhs)
    }
    buildOpInner(operands)
  }
}
