package inca.ir.analysis
import inca.ir
import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Name, Param, Ref, RefByName, Relation, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor
import inca.ir.extension.arithmetic.*
import inca.ir.util.SourceLocation

/*************************************************************************
 *  Assumptions:
 *   - no unsatisfiable atoms
 *   - no unbound Var (i.e. input was typechecked before)
 *
 *************************************************************************/



/** wraps parameters for value numbering */
case class ConfigVN(simplifyArithmetic: Boolean = false,
                    propagateConstants: Boolean = false,
                    occurrencesBeforeRemoved: Int = 0)


//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
class ValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  type ValNum = String
  type Hashed = Int

  var VN: Map[String, ValNum] = Map() // String is a Name TODO Map[Name, ValNum] ?
  var hashTable: Map[Hashed, ValNum] = Map()
  var Const: Map[String, Term] = Map() // remembers constant term assigned to Var with name string

  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  // TODO dont use scala`s hashing function
  // Type of argument -> changed from Term to Analyzable since need to Hash Atoms like Calls too
  private def getHashCode(elem: Analyzable): Hashed = elem match {
    case Var(RefByName(Name(name))) if VN.contains(name) => hashTable.find(_._2 == name).head._1
    case _ => elem.hashCode()
  }


  private var relationParams: Seq[Name] = Seq()
  private def isParam(x: String): Boolean = relationParams.map(_.name).contains(x)

  override def visitRelation(relation: Relation): Seq[Relation] = {
    relationParams = relation.params.map(_.name)
    super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] = {
    VN = Map()
    hashTable = Map()
    Const = Map()
    super.visitBody(body)
  }

  // TODO add more cases (e.g. more rules, DoubleNum)
  //  probably no recursive call needed here since called in visitTerm
  private def simplify(term: Term): Term = {
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
        case (lBinOp@BinOp(_,_,_), rhs) => BinOp(rhs, lBinOp, "+")
        case (l, r) => BinOp(l, r, "+")
      }
      case BinOp(lhs, rhs, "-") => (lhs, rhs) match {
        case (l, IntNum(0)) => l
        case (IntNum(0), r) => r
        case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(0) else BinOp(Var(Name(l)), Var(Name(r)), "-")
        case (IntNum(l), IntNum(r)) => IntNum(l - r)
        case (IntNum(l), BinOp(IntNum(r), rvar, "+")) => BinOp(IntNum(l - r), Mul(rvar,IntNum(-1)), "+")
        case (IntNum(l), BinOp(lvar,IntNum(r), "+")) => BinOp(IntNum(l - r), Mul(lvar,IntNum(-1)), "+")
        case (Var(RefByName(Name(nameL))), BinOp(IntNum(r),Var(RefByName(Name(nameR))), "+")) => if nameL==nameR then IntNum(-r) else term
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
        case (lBinOp@BinOp(_,_,_), rhs) => BinOp(rhs, lBinOp, "*")
//        case (lInt@IntNum(l), BinOp(IntNum(r),rvar,"+")) => Add(IntNum(l*r),Mul(lInt,rvar)) //  for distributivity
        case (l, BinOp(lhs,rhs,"+")) => Add(simplify(Mul(l,lhs)),simplify(Mul(l,rhs)))  //  for distributivity
        case (l, r) => BinOp(l, r, "*")
      }
      case BinOp(lhs, rhs, "/") => (lhs, rhs) match {
        case (l, IntNum(1)) => l
        case (Var(RefByName(Name(l))), Var(RefByName(Name(r)))) => if l == r then IntNum(1) else term
        case (IntNum(l), IntNum(r)) if r != 0 => IntNum(l / r)   // TODO int/int yields int in scala
        case (l, r) => BinOp(l, r, "/")
      }
      case BinOp(lhs, rhs, "%") => (lhs, rhs) match {
        case (l, IntNum(1)) => l
        case (IntNum(l), IntNum(r)) => IntNum(l % r)
        case (l, r) => BinOp(l, r, "%")
      }
      case BinOp(lhs, rhs, "min") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l < r then IntNum(l) else IntNum(r)
        case (lvar, IntNum(r)) => Min(IntNum(r),lvar)
        case (l, r) => BinOp(l, r, "min")
      }
      case BinOp(lhs, rhs, "max") => (lhs, rhs) match {
        case (IntNum(l), IntNum(r)) => if l > r then IntNum(l) else IntNum(r)
        case (lvar, IntNum(r)) => simplify(Max(IntNum(r),lvar))
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

  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = term match {
    case v@Var(RefByName(Name(name))) if Const.contains(name) && this.config.propagateConstants => Seq(Const(name))
    case v@Var(RefByName(Name(name))) if VN.contains(name) =>
      Seq(
        if Const.contains(VN(name)) && this.config.propagateConstants then
          Const(VN(name))
        else Var(RefByName(Name(VN(name)))))
    case IntNum(_) => Seq(term)
    case _ =>
      val newTerm = super.visitTerm(term).head
      val termHash: Hashed = getHashCode(newTerm)
      Seq(if (hashTable.contains(termHash)) then Var(RefByName(Name(hashTable(termHash)))) else simplify(newTerm))
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match{
    case Eq(vari@Var(RefByName(Name(x))), e, false) if vari.mode.isBinding =>   // use vari.mode.isBinding so that next case is chosen correctly
      treatBindingInEq(x,e)
    case Eq(e, vari@Var(RefByName(Name(x))), false) /*if vari.mode.isBinding*/ =>   // TODO here too?
      treatBindingInEq(x,e)
    case Eq(Var(RefByName(Name(x))), e, false) =>
      treatBindingInEq(x,e)

    case call@Call(ref, args, false) => valueNumberAtoms(call, args)
    // TODO can equivalence of two vars not be concluded from calls?
    //  could be e.g. that b(x) :- x == 0. b(x) :- x == 2. so that b(a1), b(a2) not necessarily implies that a1 == a2
//        val otherRelation = ref.target.get
//        if (otherRelation.bodies.size <= 1){  // then args which are bound by call have a unique value
//          args.map{arg =>
//           ???
//          }
//        }

    case _ => valueNumberAtoms(atom)
  }


  private def treatBindingInEq(x: String, e: Term): Seq[Atom] = {
    val newTerm = visitTerm(e).head
    val isConst = newTerm match {
      case IntNum(_) | DoubleNum(_) => true
      case _ => false
    }

    val exprHash: Hashed = getHashCode(newTerm)
    if (hashTable.contains(exprHash)) {
      val v: ValNum = newTerm match {
        case Var(RefByName(Name(str))) => str // then term was already replaced in visitTerm
        case _ => hashTable(exprHash) // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
      }
      VN += (x, v)
      // remove "Assignment" or replace term
      if isParam(x) then Seq(Eq(Var(RefByName(Name(x))), newTerm)) // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
      else Seq()
    }
    else {
      val v = x
      VN += (x, v)
      hashTable += (exprHash, v)

      if (isConst && this.config.propagateConstants) {
        Const += (x, newTerm)
        if !isParam(x) then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }

      Seq(
        // return with newTerm
        Eq(Var(RefByName(Name(x))), newTerm)
      )
    }
  }


  private def valueNumberAtoms(atom: Atom, callArgs: Seq[Arg] = Seq()): Seq[Atom] = { // callArgs only given to function if atom is a call
    val newAtom = super.visitAtom(atom).head
    val atomHash: Hashed = getHashCode(newAtom)
    val x = atom.toString

    if (hashTable.contains(atomHash)) { // TODO other Maps for Atoms or okay like this ?
      val v: ValNum = hashTable(atomHash)
      VN += (x, v)
      // remove Call or replace args
      if atom.vars.exists(arg => isParam(arg.toString)) then Seq(newAtom)
      else Seq()
    }
    else {
      val v = x
      VN += (x, v)
      hashTable += (atomHash, v)

      callArgs.foreach { case TermArg(t) => t match // add binding vars to maps
        case vari@Var(RefByName(Name(variName))) if vari.mode.isBinding => VN += (variName, variName); hashTable += (atomHash, variName)
        case _ =>
      }

      Seq(newAtom)
    }
  }


}
