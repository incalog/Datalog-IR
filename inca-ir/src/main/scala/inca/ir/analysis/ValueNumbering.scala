package inca.ir.analysis
import inca.ir
import inca.ir.{Arg, Atom, Body, Call, Eq, ExtensionalCall, Name, Param, Ref, Relation, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor
import inca.ir.extension.arithmetic.*
import inca.ir.util.SourceLocation


// TODO inherit from IROptimizer or IRVisitor?
//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
class ValueNumbering extends IRVisitor {

  type ValNum = String
  type Hashed = Int

  var VN: Map[String, ValNum] = Map() // String is a Name TODO Map[Name, ValNum] ?
  var hashTable: Map[Hashed, ValNum] = Map()

  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  // TODO dont use scala`s hashing function
  //    Type of argument -> change from Term to e.g. Analyzable since need to Hash Calls too?
  private def getHashCode(elem: Term): Hashed = elem match {
    case Var(Name(name)) if VN.contains(name) =>
        //println(VN(name))
        hashTable.find(_._2 == name).head._1
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
    super.visitBody(body)
  }

  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = term match {
    case v@Var(Name(name)) if VN.contains(name) => Seq(Var(Name(VN(name))))
//      val newVar: Var = if VN.contains(name) then Var(Name(VN(name))) else v
//      val termHash: Hashed = getHashCode(newVar)
//      Seq(if (hashTable.contains(termHash)) then Var(Name(hashTable(termHash))) else newVar)

//    case t@BinOp(lhs,rhs,op) =>
////      val newLhs = visitTerm(lhs).head
////      val newRhs = visitTerm(rhs).head
//      val newTerm = super.visitTerm(t).head
//
//      val termHash: Hashed = getHashCode(newTerm)
//      Seq(if (hashTable.contains(termHash)) then Var(Name(hashTable(termHash))) else newTerm)
    case IntNum(_) => Seq(term) // TODO ?
    case _ =>
      val newTerm = super.visitTerm(term).head
      val termHash: Hashed = getHashCode(newTerm)
      Seq(if (hashTable.contains(termHash)) then Var(Name(hashTable(termHash))) else newTerm)
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match{
    case Eq(vari@Var(Name(x)), e, false) /*if vari.mode.isBinding*/ =>   // TODO use vari.mode.isBinding so that certain comparisons will not be removed?
      val newTerm = visitTerm(e).head
      val exprHash: Hashed = getHashCode(newTerm)
      // TODO simplify
      if (hashTable.contains(exprHash)) {
        val v: ValNum = newTerm match{
          case Var(Name(str)) => str  // then term was already replaced in visitTerm
          case _ => hashTable(exprHash)  // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
        }
        VN += (x, v)
        // remove "Assignment" or replace right hand side
        if isParam(x) then Seq( Eq(Var(Name(x)), newTerm) ) // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm  TODO isParam enough ?
        else Seq()
      }
      else {
        val v = x
        VN += (x, v)
        hashTable += (exprHash, v)
        Seq(
          // return with newTerm
          Eq(Var(Name(x)), newTerm)
        )
      }
// TODO use this or not
//
//    case Eq(vari@Var(Name(x)), e, false) if !vari.mode.isBinding =>   // then not removed and not added to tables
//      val newTerm = visitTerm(e).head
//      val exprHash: Hashed = getHashCode(newTerm)
//      if (hashTable.contains(exprHash)) {
//        val v: ValNum = newTerm match {
//          case Var(Name(str)) => str // then term was already replaced in visitTerm
//          case _ => hashTable(exprHash) // term was already processed in visitTerm but there it was decided not to replace it
//        }
//        // VN += (x, v)
//        // do not remove since this Eq is not an "Assignment" and may change the meaning of the program
//        Seq(Eq(Var(Name(x)), newTerm))
//      }
//      else {
//        val v = x
//        // VN += (x, v)
//        // hashTable += (exprHash, v)
//        Seq(
//          // return with newTerm
//          Eq(Var(Name(x)), newTerm)
//        )
//      }

    case Call(ref, args, false) =>
      // TODO determine if arg is being bound -> treat those like Var in Eq above (if not neg)?
      //  but could be e.g. that b(x) :- x == 0. b(x) :- x == 2. so that b(a1) == b(a2) not necessarily implies that a1 == a2
//      val newArgs = args.flatMap(visitArg)
//      Seq(Call(ref,newArgs,false))
      val newCall = super.visitAtom(atom).head
//      val otherRelation: Relation = ref.target.get  // TODO target not known
//      if (otherRelation.bodies.size <= 1){  // then args which are bound by call have a unique value
//
////        args.map{arg =>
////          arg.vars.map{vari =>
////            ???
////          }
////        }
//
//      }
      Seq(newCall)

//    case ExtensionalCall(ref: Ref[Relation], args: Seq[Arg], neg: Boolean) => ???

    case _ => super.visitAtom(atom)
  }

}
