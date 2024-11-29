package inca.ir.valueNumbering

import inca.ir.{Term, Var}

import scala.collection.mutable


trait CongruenceClass {
  val valueId: ValueId
  var leader: Term
  var definingTerm: Term
  
  val isConstTerm: Term => Boolean
  val isParameter: Term => Boolean


  override def toString: String =
    s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm"

  def changeLeaderIfNecessary(t: Term): Boolean = { 
    if (isConstTerm(t)) {
      if (isConstTerm(leader) && leader != t) {
        return false
      }
      leader = t
    }
    if (!isParameter(leader) && !isConstTerm(leader) && isParameter(t)) {
      leader = t
    }
    return true

  }

  def changeDefTermIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Unit = {
    if (isConstTerm(t)) definingTerm = t
    else if (updateDefTermIfNecessary && definingTerm.isInstanceOf[Var] && !t.isInstanceOf[Var]) // resembles case that CongruenceClass was initially created for Var bound in Call
      definingTerm = t
  }

  def updateCongrClassIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Boolean = {
    val isValid = changeLeaderIfNecessary(t)
    if (updateDefTermIfNecessary) {
      changeDefTermIfNecessary(t)
    }
    isValid
  }

}


class CongrClassesTable {
  
  private val congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  
  def contains(vn: ValueId): Boolean = congrClasses.contains(vn)
  
  def apply(vn: ValueId): CongruenceClass = congrClasses(vn)
  
  def remove(vn: ValueId): Unit = congrClasses.remove(vn)
  
  def update(vn: ValueId, congruenceClass: CongruenceClass): Unit = congrClasses.update(vn, congruenceClass)

  override def toString: String = "\t" + congrClasses.mkString("\n\t") + "\n"
  
  def printCongrClasses(): Unit = println(this)
  
}


