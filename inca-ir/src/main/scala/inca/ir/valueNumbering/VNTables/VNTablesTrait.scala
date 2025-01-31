package inca.ir.valueNumbering.VNTables


import inca.ir.*



trait VNTablesTrait[T] (protected val congrClasses: CongrClassesTable[T], protected val valueNumbers: ValueIds[T]) {

  protected val newCongrClass: (ValueNumber, T) => CongruenceClass[T]
  
  def getCongrClasses: CongrClassesTable[T] = congrClasses
  
  def getValueNumbers: ValueIds[T] = valueNumbers
  
  def getValNumOf(t: T): ValueNumber = valueNumbers.getValNumOf(t)
  
  protected def getCongrClassOf(t: T): CongruenceClass[T] = congrClasses(getValNumOf(t))
  
  def getCongrClassOf(vn: ValueNumber): CongruenceClass[T] = congrClasses(vn)

  def isCongrClassContained(vn: ValueNumber): Boolean = congrClasses.contains(vn)
  
  def isValNumContained(t: T): Boolean = valueNumbers.contains(t)
  
  def updateValNum(t: T, vn: ValueNumber): Unit = valueNumbers.update(t, vn)
  
  def addCongrClass(congrClass: CongruenceClass[T]): Unit = congrClasses.update(congrClass.valueNumber, congrClass)

  def addCongrClass(vn: ValueNumber, leader: T): Unit = addCongrClass(newCongrClass(vn, leader))
  
  def updateCongrClassIfNecessary(vn: ValueNumber, t: T): Boolean = getCongrClassOf(vn).updateCongrClassIfNecessary(t)

  def getReplacement(t: T): T = {
    if (!congrClasses.contains(getValNumOf(t))) return t
    getCongrClassOf(t).leader
  }

  def updateValueNumbersAndCongrClasses(t: T, toVn: ValueNumber): Boolean = {
    val fromVn = getValNumOf(t)
    if (fromVn != toVn) return updateValueNumbersAndCongrClasses(fromVn, toVn)
    else if (congrClasses.contains(toVn)) {
      return updateCongrClassIfNecessary(toVn, t)
    }
    return true 
  }

  def updateValueNumbersAndCongrClasses(fromVn: ValueNumber, toVn: ValueNumber): Boolean = {
    var isValid = true 
    var updateToCongrClass = false
    if (congrClasses.contains(fromVn) && congrClasses.contains(toVn)) {
      isValid &= congrClasses(toVn).changeLeaderIfNecessary(congrClasses(fromVn).leader)
    }
    else if (congrClasses.contains(fromVn) && !congrClasses.contains(toVn)) {
      congrClasses.update(toVn, newCongrClass(toVn, congrClasses(fromVn).leader))
      valueNumbers.getAllWithVn(toVn).foreach(t => isValid &= updateCongrClassIfNecessary(toVn, t))
    }
    else if (!congrClasses.contains(fromVn) && congrClasses.contains(toVn)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithVn(fromVn).foreach(t =>
      valueNumbers.update(t,toVn)
      if (updateToCongrClass) {
        isValid &= updateCongrClassIfNecessary(toVn, t)
      }
    )
    congrClasses.remove(fromVn)
    return isValid
  }

  def printResults(): Unit = {
    println(s"Congruence Classes Info:")
    congrClasses.printCongrClasses()
    valueNumbers.printResults()
  }

}
