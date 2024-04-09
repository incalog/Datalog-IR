package inca.ir.valueNumbering

import inca.ir
import inca.ir.{Atom, Body, Call, ExtensionalCall, Name, RefByName, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor

import scala.collection.mutable


type ValueId = Int // TODO

class ValueIds{ // table from term to id
  private val ids: mutable.Map[Term,ValueId] = mutable.Map()
  private val atomIds: mutable.Map[Atom,ValueId] = mutable.Map()

  private var currentId: ValueId = 0
  private def nextId(): ValueId = {
    currentId += 1
    currentId
  }

  def getIdOf(t: Term): ValueId = ids.getOrElse(t,{
    ids.update(t,nextId())
    ids(t)
  })
  def apply(t: Term): ValueId = getIdOf(t)

  def contains(t: Term): Boolean = ids.contains(t)

  def update(t: Term, valueId: ValueId): Unit = ids.update(t, valueId)

  def getAllKeys: Seq[Term] = ids.keys.toSeq

  // TODO needed? <- can equivalence of terms in the same body be concluded from calls?
  private def getIdOf(atom: Atom): ValueId = atomIds.getOrElse(atom,{
    atomIds.update(atom,nextId())
    atomIds(atom)
  })
  def getIdOf(atom: Atom, bindingVar: Var): ValueId = /*ids.getOrElse(bindingVar,*/{
    case class bindingArg() extends Term { // serves as a marker which argument is currently binding
      override def vars: Seq[Var] = Seq()
    }
    //ids.update(bindingVar,
      atom match {
        case Call(ref, args, neg) =>
          val argsFiltered = args.patch(args.indexOf(TermArg(bindingVar)), Seq(TermArg(bindingArg())), 1)
          getIdOf(Call(ref, argsFiltered, neg))
        case ExtensionalCall(ref, args, neg) =>
          val argsFiltered = args.patch(args.indexOf(TermArg(bindingVar)), Seq(TermArg(bindingArg())), 1)
          getIdOf(ExtensionalCall(ref, argsFiltered, neg))
        case _ => throw new IllegalArgumentException("This should not happen")
    }//)
    //ids(bindingVar)
  }//)

  override def toString: String = ids.toString() + "\n" + atomIds.toString()
}


case class CongruenceClass(valueId: ValueId, var leader: Term, definingTerm: Term, var contents: Seq[Term]) // contents just saved for debugging and presentation



trait BaseValueNumberingNew(config: ConfigVN = ConfigVN()) extends IRVisitor {
  /* def valueNumberTerm(term): Term


   */

  /* VN(x) -> congrClasses(valueNumbers(x)).leader -> getReplacementTerm(x)
  *  VN += (x, v) -> valueNumbers.update(x, valueNumbers(v)) -> ??? update congruence class if x is a const
  * */
  /* hashTable(n) -> congrClasses(n).leader
     hashTable += (termId,v) -> change leader or create new congruence class
            -> congrClasses(termId).leader = v;  congrClasses.update(termId, new CongruenceClass(termId,v,v,Seq(v)) )
   */

  private val congrClasses: mutable.Map[ValueId,CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds = new ValueIds() // Map[Term, ValueId]

  def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
  def getReplacementTerm(t: Term): Term = getCongrClassOf(t).leader


// TODO is that needed if constants are always used as leader and thus propagated when possible?
//  -> if it is not (replaced by) a constant its value is unknown
// var valueUnknown: mutable.Map[Var, mutable.Set[Var]] = mutable.Map() // remembers variables that where bound in calls -> if they are compared in Eq those shouldnt be removed


  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  // reset congrClasses (otherwise would need to make sure that globally every Var has a different name)
  override def visitBody(body: Body): Seq[Body] = super.visitBody(body)

  /* for each Eq(lhs,rhs,true) in which lhs is a binding Var:  // same if rhs is binding
          normalizedTerm = normalize(rhs) // also potentially replace subterms
          termId = getIdOf(normalizedTerm)
          if termId in CongrClasses then
              add lhs to CongrClasses(termId)
              valueNumbers.update(lhs, termId)
              remove Eq
          else:
              newCongrClass = new CongruenceClass(termId, lhs, rhs, Seq(lhs,rhs))
              add newCongrClass to CongrClasses(termId)
              leave Eq in program
   */
  override def visitAtom(atom: Atom): Seq[Atom] = super.visitAtom(atom)

  /* for each term in body:
       normalizedTerm = normalize(term)
       termId = getIdOf(normalizedTerm)   // -> adds normalizedTerm to ValueIds if not present before
       if termId in CongrClasses then
           congrClass = congrClasses(termId)
           replace term with congrClass.leader
       else:
           add normalizedTerm with termId to congrClasses
  */
  override def visitTerm(term: Term): Seq[Term] = super.visitTerm(term)

  def normalize(term: Term): Term = term

  def isConst(term: Term): Boolean = false





}