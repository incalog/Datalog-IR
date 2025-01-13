package inca.ir.valueNumbering.VNTables


import inca.ir.*



trait VNTablesTrait[T] (protected val congrClasses: CongrClassesTable[T], protected val valueNumbers: ValueIds[T]) {

  protected val newCongrClass: (ValueId, T) => CongruenceClass[T]
  
  def getCongrClasses: CongrClassesTable[T] = congrClasses
  
  def getValueNumbers: ValueIds[T] = valueNumbers
  
  def getIdOf(t: T): ValueId = valueNumbers.getIdOf(t)
  
  protected def getCongrClassOf(t: T): CongruenceClass[T] = congrClasses(getIdOf(t))
  
  def getCongrClassOf(vn: ValueId): CongruenceClass[T] = congrClasses(vn)

  def isCongrClassContained(vn: ValueId): Boolean = congrClasses.contains(vn)
  
  def isValNumContained(t: T): Boolean = valueNumbers.contains(t)
  
  def updateValNum(t: T, vn: ValueId): Unit = valueNumbers.update(t, vn)
  
  def addCongrClass(congrClass: CongruenceClass[T]): Unit = congrClasses.update(congrClass.valueId, congrClass)

  def addCongrClass(vn: ValueId, leader: T): Unit = addCongrClass(newCongrClass(vn, leader))
  
  def updateCongrClassIfNecessary(vn: ValueId, t: T): Boolean = getCongrClassOf(vn).updateCongrClassIfNecessary(t)

  def getReplacement(t: T): T = {
    if (!congrClasses.contains(getIdOf(t))) return t
    getCongrClassOf(t).leader
  }

  def updateValueNumbersAndCongrClasses(t: T, toId: ValueId): Boolean = {
    val fromId = getIdOf(t)
    if (fromId != toId) return updateValueNumbersAndCongrClasses(fromId, toId)
    else if (congrClasses.contains(toId)) {
      return updateCongrClassIfNecessary(toId, t)
    }
    return true 
  }

  def updateValueNumbersAndCongrClasses(fromId: ValueId, toId: ValueId): Boolean = {
    var isValid = true 
    var updateToCongrClass = false
    if (congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      isValid &= congrClasses(toId).changeLeaderIfNecessary(congrClasses(fromId).leader)
    }
    else if (congrClasses.contains(fromId) && !congrClasses.contains(toId)) {
      congrClasses.update(toId, newCongrClass(toId, congrClasses(fromId).leader))
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
