package inca.ir.analysis
import inca.ir
import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Name, Param, Ref, Relation, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor
import inca.ir.extension.arithmetic.*


// TODO inherit from IROptimizer or IRVisitor?
//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
class ValueNumbering extends IRVisitor {

  type ValNum = String
  type Hashed = Int

  var VN: Map[String, ValNum] = Map()
  var hashTable: Map[Hashed, ValNum] = Map()

  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  // TODO dont use scala`s hashing function
  private def getHashCode(term: Term): Hashed = term match {
    case Var(Name(name)) if VN.contains(name) =>
        //println(VN(name))
        hashTable.find(_._2 == name).head._1
    case _ => term.hashCode()
  }


  private var relationParams: Seq[Param] = Seq()
  private def isParam(x: String): Boolean = relationParams.map(_.name.name).contains(x)

  override def visitRelation(relation: Relation): Seq[Relation] = {
    relationParams = relation.params
    super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] = {
    VN = Map()
    hashTable = Map()
    super.visitBody(body)
  }

  override def visitTerm(term: Term): Seq[Term] = term match { // replace
    // case BinOp(rhs, lhs, sym) => BinOp(visitTerm(rhs), visitTerm(lhs), sym)
    case Var(Name(name)) if VN.contains(name) => Seq(Var(Name(VN(name))))
    case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match{
    case Eq(Var(Name(x)), e, false) =>
      val newTerm = visitTerm(e).head
      val exprHash: Hashed = getHashCode(newTerm)
      // TODO simplify ???
      if (hashTable.contains(exprHash)) {
        val v = hashTable(exprHash)
        VN += (x, v)
        // remove Assignment or replace right hand side of assignment
        if isParam(x) then Seq( Eq(Var(Name(x)), Var(Name(v))) ) // TODO isParam enough ?
        else Seq()
      }
      else {
        val v = x
        VN += (x, v)
        hashTable += (exprHash, v)
        Seq(
          // return with newTerm -> assignment may be removed
          Eq(Var(Name(x)), newTerm)
        )
      }
    // case Eq(lhs, rhs, true) => Seq(Eq(visitTerm(lhs).head, visitTerm(rhs).head, true))

    case Call(ref, args, false) =>
      // TODO how to determine if arg is being bound? -> treat those like Var in Eq above (if not neg)
      val newArgs = args.flatMap(visitArg)
      Seq(Call(ref,newArgs,false))
//    case ExtensionalCall(ref: Ref[Relation], args: Seq[Arg], neg: Boolean) => ???

    case BinCompare(lhs@Var(Name(x)), rhs, op) => // TODO hash lhs here too?
      // replace Term with Variable if already computed
      val newTermL = visitTerm(lhs).head
      val newTermR = visitTerm(rhs).head
      val exprHash: Hashed = getHashCode(newTermR)
      val newRhs = if (hashTable.contains(exprHash)) then Var(Name(hashTable(exprHash))) else newTermR
      Seq(BinCompare(newTermL, newRhs,op))

    case _ => super.visitAtom(atom)
  }

}
