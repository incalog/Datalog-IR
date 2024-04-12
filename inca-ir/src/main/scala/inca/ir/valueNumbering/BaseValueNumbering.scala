package inca.ir.valueNumbering

import inca.ir
import inca.ir.{Atom, Body, Call, ExtensionalCall, Name, RefByName, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor

import scala.collection.mutable


type ValueId = Int // TODO BigInt or String but should be okay to use Int (throw an exception if overflow)

class ValueIds[T]{ // table from term to id
  private val ids: mutable.Map[T,ValueId] = mutable.Map()

  private var currentId: ValueId = 0 //Int.MinValue
  private def nextId(): ValueId = {
    if (currentId == Int.MaxValue){
      throw IllegalStateException("Too many CongruenceClasses: Overflow in ValueIds")
    }
    currentId += 1
    currentId
  }

  def getIdOf(t: T): ValueId = ids.getOrElse(t,{
    ids.update(t,nextId())
    ids(t)
  })
  def apply(t: T): ValueId = getIdOf(t)

  def contains(t: T): Boolean = ids.contains(t)

  def update(t: T, valueId: ValueId): Unit = ids.update(t, valueId)

  def clear(): Unit = {
    currentId = 0
    ids.clear()
  }

  override def toString: String = "IDs: \t\t\t" + ids.mkString(";  ")

  // just for printing and debugging (constructing congrClasses like this every time is too computationally complex)
  private def congrClasses: Map[ValueId, Seq[T]] = ids.groupBy(_._2).map((id, m) => id -> m.keys.toSeq)

  def printCongrClasses(): Unit = println("CongrClasses: \t" + congrClasses.mkString(";\n\t\t\t\t"))

  def printResults(): Unit = {
    println("VN Results: ")
    println(this)
    printCongrClasses()
    println("")
  }

}






trait BaseValueNumberingNew(config: ConfigVN = ConfigVN()) extends IRVisitor {

  case class CongruenceClass(valueId: ValueId, var leader: Term, definingTerm: Term, var contents: Seq[Term]) { // contents just saved for debugging
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm, contents = $contents"

    def add(t: Term): Unit = {
      if (isConst(t)) leader = t
      contents = contents.appended(t)
    }

  }

  private val congrClasses: mutable.Map[ValueId,CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds[Term] = new ValueIds() // Map[Term, ValueId]

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
      // reset congrClasses (otherwise not known when variables are unbound)
      congrClasses.clear()
      Seq(resBody)
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
    override def visitAtom(atom: Atom): Seq[Atom] = ???

    /* for each term in body:
         if const return
         if already known replace with congrClass.leader
         visit subterms
         normalizedTerm = normalize(term)
         termId = getIdOf(normalizedTerm)   // -> adds normalizedTerm to ValueIds if not present before
         if termId in CongrClasses then
             congrClass = congrClasses(termId)
             replace term with congrClass.leader
         else:
             add normalizedTerm with termId to congrClasses
    */
    override def visitTerm(term: Term): Seq[Term] = ???

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