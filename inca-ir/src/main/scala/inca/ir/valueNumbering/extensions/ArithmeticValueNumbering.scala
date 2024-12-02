package inca.ir.valueNumbering.extensions

import inca.ir.valueNumbering.BaseValueNumbering
import inca.ir.{Term, TermType, Var}
import inca.ir.extension.arithmetic.*
import inca.ir.typing.Mode.Bound



trait ArithmeticValueNumbering extends BaseValueNumbering {

  // when double terms not normalized then not allowed to replace non-variable terms
  // otherwise terms that would be constant in Eqs that bind a variable replaced but var still leader -> unbound in other atoms
  override def isAllowedToReplace(term: Term): Boolean = {
    if (term.typ.get.ty == TDouble && !normalizeDoubles) return term.isInstanceOf[Var]
    else return super.isAllowedToReplace(term)
  }

  protected override def isConst(term: Term): Boolean = term match {
    case IntNum(_) | DoubleNum(_) => true
    case _ => super.isConst(term)
  }

  // in the beginning no recursive call needed here since called in visitTerm ->  all subterms visited already
  protected override def normalize(term: Term): Term = {
    val typ = term.typ match { // assumed that program was typechecked before and every term thus has a type
      case Some(termType: TermType) => termType
      case _ => throw new IllegalStateException(s"Untyped Term $term in normalization")
    }
    if (typ.ty == TDouble && !normalizeDoubles) return term

    val newTerm = term match {
      case BinOp(lhs, rhs, "+") => normalizeAdd(lhs, rhs, typ)
      case BinOp(lhs, rhs, "-") => normalizeSub(lhs, rhs, typ)
      case BinOp(lhs, rhs, "*") => normalizeMul(lhs, rhs, typ)
      case BinOp(lhs, rhs, "/") => normalizeDiv(lhs, rhs, typ)
      case BinOp(lhs, rhs, "%") => normalizeRemainder(lhs,rhs,typ)
      case BinOp(lhs, rhs, "min") => normalizeMin(lhs,rhs,typ)
      case BinOp(lhs, rhs, "max") => normalizeMax(lhs,rhs,typ)
      case UnOp(t, "abs") => normalizeAbs(t,typ)
      case UnOp(t, "-") => normalizeNeg(t,typ)
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
    // without visitTerm defterm might contain removed Var
    if (this.useDefiningTerm && !isConst(t)) { // TODO use interface when to outline/inline term (make sure t not contained in defterm)
      val newTerm =  visitTerm(vnTables.getDefiningTerm(t)).head.typed(t.typ.get, force = true)
      return newTerm
    }
    else return t
  }

  private def newIntNum(i: Int): IntNum = IntNum(i).typed(TermType(TInt,Bound))
  private def newDoubleNum(d: Double): DoubleNum = DoubleNum(d).typed(TermType(TDouble,Bound))

  private def normalizeAdd(lhs: Term, rhs: Term, typ: TermType, repetition: Boolean = false): Term = getArgumentsOfOp(lhs,rhs) match {
    case (l, IntNum(0) | DoubleNum(0)) => l
    case (IntNum(0) | DoubleNum(0), r) => r
    case (IntNum(l), IntNum(r)) => newIntNum(l + r)
    case (DoubleNum(l), DoubleNum(r)) => newDoubleNum(l + r)
  
    case (term, BinOp(IntNum(-1), termNeg, "*")) if getIdOf(term) == getIdOf(termNeg) => newIntNum(0)
    case (term, BinOp(DoubleNum(-1), termNeg, "*")) if getIdOf(term) == getIdOf(termNeg) => newDoubleNum(0)
    case (BinOp(IntNum(-1), termNeg, "*"), term) if getIdOf(term) == getIdOf(termNeg) => newIntNum(0)
    case (BinOp(DoubleNum(-1), termNeg, "*"), term) if getIdOf(term) == getIdOf(termNeg) => newDoubleNum(0)
  
    case (lTerm, BinOp(BinOp(IntNum(-1) | DoubleNum(-1), l, "*"), r, "+")) if getIdOf(lTerm) == getIdOf(l) => r
    case (lTerm, BinOp(l, BinOp(IntNum(-1) | DoubleNum(-1), r, "*"), "+")) if getIdOf(lTerm) == getIdOf(r) => l
  
    case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ.ty == TInt => normalize(Mul(newIntNum(2), lhs).typed(typ)) // x + x == 2*x for Ints
    case (lhs, BinOp(l, r, "+")) if getIdOf(lhs) == getIdOf(l) && typ.ty == TInt =>  normalize(Add(normalize(Mul(newIntNum(2), lhs).typed(typ)),r).typed(typ))
    case (lhs, BinOp(IntNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(newIntNum(x + 1), lhs).typed(typ))
    case (lhs: Term, rhs: Term) if getIdOf(lhs) == getIdOf(rhs) && typ.ty == TDouble => normalize(Mul(newDoubleNum(2), lhs).typed(typ)) // for Doubles
    case (lhs, BinOp(l, r, "+")) if getIdOf(lhs) == getIdOf(l) && typ.ty == TDouble =>  normalize(Add(normalize(Mul(newDoubleNum(2), lhs).typed(typ)),r).typed(typ))
    case (lhs, BinOp(DoubleNum(x), rhs, "*")) if getIdOf(lhs) == getIdOf(rhs) => normalize(Mul(newDoubleNum(x + 1), lhs).typed(typ))
  
    case (l, r) => if !repetition then orderAssociativityCommutativity(getAllOperands(Add(l,r),"+"), typ, "+") else Add(l,r)
  }

  private def normalizeSub(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match { // fold or rewrite to Add(lhs, Mul(-1, rhs))
    case (l, IntNum(0)) => l
    case (l, DoubleNum(0)) => l
    case (IntNum(0), r) => normalize(Mul(newIntNum(-1), r).typed(typ))
    case (DoubleNum(0), r) => normalize(Mul(newDoubleNum(-1), r).typed(typ))
    case (l, r) if getIdOf(l) == getIdOf(r) =>
      if typ.ty == TInt then newIntNum(0)
      else if typ.ty == TDouble then newDoubleNum(0)
      else throw new IllegalStateException(getExceptionMsg("Sub", lhs, rhs, typ))
    case (IntNum(l), IntNum(r)) => newIntNum(l - r)
    case (DoubleNum(l), DoubleNum(r)) => newDoubleNum(l - r)
  
    case (l, r) =>
      if typ.ty == TInt then normalize(Add(l, normalize(Mul(newIntNum(-1),r).typed(typ)) ).typed(typ))
      else if typ.ty == TDouble then normalize(Add(l, normalize(Mul(newDoubleNum(-1),r).typed(typ)) ).typed(typ))
      else throw new IllegalStateException(getExceptionMsg("Sub", lhs, rhs, typ))
    }

  private def normalizeMul(lhs: Term, rhs: Term, typ: TermType, repetition: Boolean = false): Term = getArgumentsOfOp(lhs,rhs) match {
    case (_, IntNum(0)) | (IntNum(0), _) => newIntNum(0)
    case (_, DoubleNum(0)) | (DoubleNum(0), _) => newDoubleNum(0)
    case (IntNum(1) | DoubleNum(1), r) => r
    case (l, IntNum(1) | DoubleNum(1)) => l
    case (IntNum(l), IntNum(r)) => newIntNum(l * r)
    case (DoubleNum(l), DoubleNum(r)) => newDoubleNum(l * r)

    case (factor, BinOp(l, r, "+")) => distributivity(factor,l,r,Add,Mul,typ)
    case (BinOp(l, r, "+"), factor) => distributivity(factor,l,r,Add,Mul,typ)

    // no such rewriting for TInt since a * (b / a) = 0 if a > b
    case (lTerm, BinOp(l, rTerm, "/")) if getIdOf(lTerm) == getIdOf(rTerm) && typ.ty == TDouble => l

    case (l, r) => if !repetition then orderAssociativityCommutativity(getAllOperands(Mul(l,r),"*"), typ, "*") else Mul(l,r)
  }

  private def normalizeDiv(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
    case (l,r) if vnTables.getReplacement(r) == IntNum(0) || vnTables.getReplacement(r) == DoubleNum(0) => Div(l,r)
    case (l, IntNum(1) | DoubleNum(1)) => l
    case (l, r) if getIdOf(l) == getIdOf(r) =>
      if typ.ty == TInt then newIntNum(1)
      else if typ.ty == TDouble then newDoubleNum(1)
      else throw new IllegalStateException(getExceptionMsg("Div", lhs, rhs, typ))
    case (IntNum(l), IntNum(r)) if r != 0 => newIntNum(l / r) // int/int yields int in scala
    case (DoubleNum(l), DoubleNum(r)) if r != 0 => newDoubleNum(l / r)

    case (lBinOp@BinOp(l, lTerm, "*"), rTerm) if getIdOf(lTerm) == getIdOf(rTerm) => l
    case (lBinOp@BinOp(lTerm, l, "*"), rTerm) if getIdOf(lTerm) == getIdOf(rTerm) => l
    case (lTerm, lBinOp@BinOp(r, rTerm, "*")) if getIdOf(lTerm) == getIdOf(rTerm) && typ.ty == TDouble => normalize(Div(newDoubleNum(1), r).typed(typ))
    case (lTerm, lBinOp@BinOp(rTerm, r, "*")) if getIdOf(lTerm) == getIdOf(rTerm) && typ.ty == TDouble => normalize(Div(newDoubleNum(1), r).typed(typ))

    case (BinOp(l, r, "+"), denom) if typ.ty == TDouble => distributivity(denom,l,r,Add,Div,typ)

    case (l, r) => Div(l,r)
  }

  private def normalizeRemainder(lhs: Term, rhs: Term, typ: TermType): Term = getArgumentsOfOp(lhs,rhs) match {
    case (l, IntNum(1) | DoubleNum(1)) => l
    case (IntNum(0),_) => newIntNum(0)
    case (DoubleNum(0),_) => newDoubleNum(0)
    case (IntNum(l), IntNum(r)) => newIntNum(l % r)
    case (DoubleNum(l), DoubleNum(r)) => newDoubleNum(l % r)

    case (lhs,rhs) if getIdOf(lhs) == getIdOf(rhs) =>
      if typ.ty == TInt then newIntNum(0)
      else if typ.ty == TDouble then newDoubleNum(0)
      else throw new IllegalStateException(getExceptionMsg("Remainder",lhs,rhs,typ))

    case (BinOp(l,r,"*"),rhs) if getIdOf(l) == getIdOf(rhs) =>
      if typ.ty == TInt then newIntNum(0)
      else if typ.ty == TDouble then newDoubleNum(0)
      else throw new IllegalStateException(getExceptionMsg("remainder",lhs,rhs,typ))

    case (BinOp(l,r,"*"),rhs) if getIdOf(r) == getIdOf(rhs) =>
      if typ.ty == TInt then newIntNum(0)
      else if typ == TDouble then newDoubleNum(0)
      else throw new IllegalStateException(getExceptionMsg("remainder",lhs,rhs,typ))

    case (l, r) => Remainder(l,r)
  }

  private def normalizeMin(lhs: Term, rhs: Term, typ: TermType, repetition: Boolean = false): Term = getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l < r then newIntNum(l) else newIntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l < r then newDoubleNum(l) else newDoubleNum(r)
    case (l,r) if getIdOf(l) == getIdOf(r) => l
    case (l, r) => if !repetition then orderAssociativityCommutativity(getAllOperands(Min(l,r),"min"), typ, "min") else Min(l,r)
  }

  private def normalizeMax(lhs: Term, rhs: Term, typ: TermType): Term =  getArgumentsOfOp(lhs, rhs) match {
    case (IntNum(l), IntNum(r)) => if l > r then newIntNum(l) else newIntNum(r)
    case (DoubleNum(l), DoubleNum(r)) => if l > r then newDoubleNum(l) else newDoubleNum(r)
    case (l,r) if getIdOf(l) == getIdOf(r) => l
    case (l, r) =>
      if (typ.ty == TInt)
        normalize(Mul(newIntNum(-1), normalize(Min(normalize(Mul(newIntNum(-1), l).typed(typ)), normalize(Mul(newIntNum(-1), r).typed(typ))).typed(typ))).typed(typ))
      else if (typ.ty == TDouble)
        normalize(Mul(newDoubleNum(-1), normalize(Min(normalize(Mul(newDoubleNum(-1), l).typed(typ)), normalize(Mul(newDoubleNum(-1), r).typed(typ))).typed(typ))).typed(typ))
      else throw new IllegalStateException(getExceptionMsg("max",lhs,rhs,typ))
  }

  private def normalizeAbs(t: Term, typ: TermType): Term = getArgumentOfOp(t) match {
    case IntNum(value) => if value >= 0 then newIntNum(value) else newIntNum(-1 * value)
    case DoubleNum(value) => if value >= 0 then newDoubleNum(value) else newDoubleNum(-1 * value)
    case abs@UnOp(_, "abs") => abs
    case arg => Abs(arg)
  }

  private def normalizeNeg(t: Term, typ: TermType): Term = getArgumentOfOp(t) match {
    case IntNum(value) => newIntNum(-1 * value)
    case DoubleNum(value) => newDoubleNum(-1 * value)
    case term =>
      if (typ.ty == TInt) normalize(Sub(newIntNum(0), term).typed(typ))
      else if (typ.ty == TDouble) normalize(Sub(newDoubleNum(0), term).typed(typ))
      else throw new IllegalStateException(s"unknown type $typ in normalization of Neg with $term")
  }

  
  private def getExceptionMsg(op: String, lhs: Term, rhs: Term, typ: TermType): String =
    s"unknown type $typ in normalization of $op with $lhs and $rhs"

//  case (factor, BinOp(l, r, "+")) => distributivity(factor,l,r,Add,Mul,typ)
  private def distributivity(factor: Term, lhs: Term, rhs: Term, opOuter: (Term, Term) => BinOp, opInner: (Term, Term) => BinOp, typ: TermType): Term = {
    val innerL = opInner(lhs, factor).typed(typ)
    val innerR = opInner(rhs, factor).typed(typ)
    val res = opOuter(normalize(innerL), normalize(innerR)).typed(typ)
    val result = normalize(res)
    println(result)
    result
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
      case "min" => typ.ty match {
        case TInt => Int.MaxValue
        case TDouble => Double.MaxValue
      }
    }
    val number = (typ.ty, op) match {
      case (TInt, "+") => newIntNum(operands.filter(_.isInstanceOf[IntNum]).foldRight(neutralElem.toInt) { case (IntNum(n1), n2) => n1 + n2 })
      case (TDouble, "+") => newDoubleNum(operands.filter(_.isInstanceOf[DoubleNum]).foldRight(neutralElem.toDouble) { case (DoubleNum(n1), n2) => n1 + n2 })
      case (TInt, "*") => newIntNum(operands.filter(_.isInstanceOf[IntNum]).foldRight(neutralElem.toInt) { case (IntNum(n1), n2) => n1 * n2 })
      case (TDouble, "*") => newDoubleNum(operands.filter(_.isInstanceOf[DoubleNum]).foldRight(neutralElem.toDouble) { case (DoubleNum(n1), n2) => n1 * n2 })
      case (TInt, "min") => newIntNum(operands.filter(_.isInstanceOf[IntNum]).foldRight(neutralElem.toInt) { case (IntNum(n1), n2) => math.min(n1, n2) })
      case (TDouble, "min") => newDoubleNum(operands.filter(_.isInstanceOf[DoubleNum]).foldRight(neutralElem.toDouble) { case (DoubleNum(n1), n2) => math.min(n1,n2) })
    }
    val vars = operands.filter{ // Vars and negated Vars
      case Var(_) => true
      case BinOp(IntNum(-1),Var(_), "*") | BinOp(DoubleNum(-1),Var(_), "*") => true
      case _ => false
    }.sortWith {
      case (v1@Var(_), v2@Var(_)) => v1.name.name < v2.name.name
      case (v1@Var(_), BinOp(_, v2@Var(_), _)) => v1.name.name < v2.name.name
      case (BinOp(_, v1@Var(_), _), v2@Var(_)) => v1.name.name < v2.name.name
      case (BinOp(_, v1@Var(_), _), BinOp(_, v2@Var(_), _)) => v1.name.name < v2.name.name
    }
    val abss = operands.filter {
      case UnOp(_, "abs") => true
      case _ => false
    }
    val adds = if (op != "+" && op != "*") operands.filter {
      case BinOp(_, _, "+") => true
      case _ => false
    } else Seq()

    val muls = if (op != "*") operands.filter {
      case BinOp(IntNum(-1),Var(_), "*") | BinOp(DoubleNum(-1),Var(_), "*") => false
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
    val mins = if (op != "min") operands.filter {
      case BinOp(_, _, "min") => true
      case _ => false
    } else Seq()

    val newOperands = (if number != IntNum(neutralElem.toInt) && number != DoubleNum(neutralElem) then Seq(number) else Seq())
      ++ vars ++ Seq(abss, adds, muls, divs, remains, mins).flatMap(sortedByID)
    val result = buildOp(newOperands, typ, op)
    result
  }

  private def buildOp(operands: Seq[Term], typ: TermType, op: String): Term = {
    val binOp = (l, r) => BinOp(l, r, op)
    val normOp = op match {
      case "+" => normalizeAdd(_, _, typ, repetition = true)
      case "*" => normalizeMul(_, _, typ, repetition = true)
      case "min" => normalizeMin(_, _, typ, repetition = true)
    }

    def buildOpInner(operands: Seq[Term]): Term = operands match {
      case Nil => throw new IllegalStateException(s"Normalization: no terms in operation")
      case head :: Nil => head
      case h1 :: h2 :: tail =>
        val recRes = buildOpInner(h2 :: tail).typed(typ, force = true)
        val newRes = binOp(h1, recRes).typed(typ)
        normOp(newRes.lhs, newRes.rhs)
    }
    buildOpInner(operands)
  }
}
