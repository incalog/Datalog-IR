package inca.ir.valueNumbering

import inca.ir.{Name, RefByName, Term, TermType, Var}


trait VNTablesTrait {
  private var congrClasses: CongrClassesTable = CongrClassesTable()
  private var valueNumbers: ValueIds[Term] = new ValueIds()

  protected val newCongrClass: (ValueId, Term, Term) => CongruenceClass
  
  def getCongrClasses: CongrClassesTable = congrClasses
  
  def getValueNumbers: ValueIds[Term] = valueNumbers
  
  def getIdOf(t: Term): ValueId = valueNumbers.getIdOf(t)
  
  private def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
  
  def getCongrClassOf(vn: ValueId): CongruenceClass = congrClasses(vn)

  def isCongrClassContained(vn: ValueId): Boolean = congrClasses.contains(vn)
  
  def isValNumContained(term: Term): Boolean = valueNumbers.contains(term)
  
  def updateValNum(term: Term, vn: ValueId): Unit = valueNumbers.update(term, vn)
  
  def addCongrClass(congrClass: CongruenceClass): Unit = congrClasses.update(congrClass.valueId, congrClass)
  
  def updateCongrClassIfNecessary(vn: ValueId, term: Term, updateDefTermIfNecessary: Boolean = false): Boolean = 
    getCongrClassOf(vn).updateCongrClassIfNecessary(term)

  def getReplacementTerm(t: Term): Term = {
    if (!congrClasses.contains(valueNumbers(t))) return t

    val leader = getCongrClassOf(t).leader
    leader match {
      case vari@Var(_) => VNTablesTrait.newVar(vari.name,t.typ)
      case _ => leader
    }
  }

  def getDefiningTerm(t: Term): Term = {
    if (valueNumbers.contains(t)){
      if (congrClasses.contains(valueNumbers(t))) {
        return getCongrClassOf(t).definingTerm
      }
    }
    return t
  }

  def updateValueNumbersAndCongrClasses(term: Term, toId: ValueId): Boolean = {
    val fromId = getIdOf(term)
    if (fromId != toId) return updateValueNumbersAndCongrClasses(fromId, toId)
    else if (congrClasses.contains(toId)) {
      return updateCongrClassIfNecessary(toId, term)
    }
    return true 
  }

  def updateValueNumbersAndCongrClasses(fromId: ValueId, toId: ValueId): Boolean = {
    var isValid = true 
    var updateToCongrClass = false
    if (congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      isValid &= congrClasses(toId).changeLeaderIfNecessary(congrClasses(fromId).leader)
      congrClasses(toId).changeDefTermIfNecessary(congrClasses(fromId).definingTerm)
    }
    else if (congrClasses.contains(fromId) && !congrClasses.contains(toId)) {
      congrClasses.update(toId, newCongrClass(toId, congrClasses(fromId).leader, congrClasses(fromId).definingTerm))
      valueNumbers.getAllWithId(toId).foreach(t => isValid &= updateCongrClassIfNecessary(toId, t))
    }
    else if (!congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithId(fromId).foreach(t =>
      valueNumbers.update(t,toId)
      if (updateToCongrClass) {
        isValid &= updateCongrClassIfNecessary(toId, t)
      }
    )
    congrClasses.remove(fromId)
    return isValid
  }

  def printResults(): Unit = {
    println(s"Congruence Classes Info:")
    congrClasses.printCongrClasses()
    valueNumbers.printResults()
  }

}



object VNTablesTrait {
  def newVar(name: Name, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(name))
    v.typ = ty
    v
  }
  
  def newVNTableWith(VNs: ValueIds[Term], congrClassesTable: CongrClassesTable, constructor: () => VNTablesTrait): VNTablesTrait = {
    val table = constructor()
    table.valueNumbers = VNs
    table.congrClasses = congrClassesTable
    table
  }
  
}