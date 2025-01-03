package inca.ir.valueNumbering.VNTables


import inca.ir.{Relation, Term, Var}

import scala.collection.mutable


trait CongruenceClass[T] {
  val valueId: ValueId
  var leader: T

  override def toString: String =
    s"Congruence Class: Id = $valueId, leader = $leader"

  def changeLeaderIfNecessary(t: T): Boolean 
  
  def updateCongrClassIfNecessary(t: T): Boolean = changeLeaderIfNecessary(t)
  
}



trait CongruenceClassTerms extends CongruenceClass[Term] {
  var definingTerm: Term

  val isConstTerm: Term => Boolean
  val isParameter: Term => Boolean

  override def toString: String = s"${super.toString}, definingTerm = $definingTerm"

  override def changeLeaderIfNecessary(t: Term): Boolean = {
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


case class CongruenceClassRelations(valueId: ValueId, var leader: Relation) extends CongruenceClass[Relation] {

  override def changeLeaderIfNecessary(t: Relation): Boolean = false // TODO if never necessary move function to terms

}



class CongrClassesTable[T] {
  
  protected val congrClasses: mutable.Map[ValueId, CongruenceClass[T]] = mutable.Map()
  
  def contains(vn: ValueId): Boolean = congrClasses.contains(vn)
  
  def apply(vn: ValueId): CongruenceClass[T] = congrClasses(vn)
  
  def remove(vn: ValueId): Unit = congrClasses.remove(vn)
  
  def update(vn: ValueId, congruenceClass: CongruenceClass[T]): Unit = congrClasses.update(vn, congruenceClass)

  override def toString: String = "\t" + congrClasses.mkString("\n\t") + "\n"
  
  def printCongrClasses(): Unit = println(this)
  
}


