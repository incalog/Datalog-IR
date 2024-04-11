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

  override def toString: String = "IDs: \t\t\t" + ids.mkString(";  ") + "\natomIDs: \t\t" + atomIds.mkString(";\t ")
  // just for printing and debugging (constructing congrClasses like this every time is too computationally complex)
  private def congrClasses: Map[ValueId,Seq[Term]] = ids.groupBy(_._2).map((id,m) => id -> m.keys.toSeq)
  def printCongrClasses(): Unit = println("CongrClasses: \t" + congrClasses.mkString(";\n\t\t\t\t"))
  def printResults(): Unit = {
    println("VN Results: ")
    println(this)
    printCongrClasses()
    println("")
  }
}


case class CongruenceClass(valueId: ValueId, var leader: Term, definingTerm: Term, var contents: Seq[Term]) // contents just saved for debugging and presentation



trait BaseValueNumberingNew(config: ConfigVN = ConfigVN()) extends IRVisitor {

  private val congrClasses: mutable.Map[ValueId,CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds = new ValueIds() // Map[Term, ValueId]

  private def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
  private def getReplacementTerm(t: Term): Term = getCongrClassOf(t).leader


  def valueNumbering(module: ir.Module): ir.Module = {
//    val newModule = ValueNumberingAnalyze().visitModule(module)
//    ValueNumberingRewrite().visitModule(newModule)
    super.visitModule(module)
  }

  def normalize(term: Term): Term = term

  def isConst(term: Term): Boolean = false

  //class ValueNumberingAnalyze extends IRVisitor {


    override def visitBody(body: Body): Seq[Body] = {
      val newBody = super.visitBody(body).head // normalization and analyzation
      val resBody = ValueNumberingRewrite().rewriteBody(newBody) // rewrite
      Seq(resBody)
      // reset congrClasses (otherwise not known when variables are unbound)
//      congrClasses.clear()
    }

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

    private def valueNumberVar(vari: Var, t: Term): Unit = ???

  //}

  class ValueNumberingRewrite extends IRVisitor{
//    def rewrite(module: ir.Module): ir.Module = super.visitModule(module)

    //override protected def visitBody(body: Body): Body = ???
    def rewriteBody(body: Body): Body = super.visitBody(body).head

    //override protected def visitAtom(atom: Atom): Seq[Atom] = ???
    private def rewriteAtom(atom: Atom): Seq[Atom] = ???


    private def rewriteTerm(term: Term): Seq[Term] = ???

  }



}