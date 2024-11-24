package inca.ir.valueNumbering

import inca.ir.Term

trait CongruenceClass {
  val valueId: ValueId
  var leader: Term
  var definingTerm: Term

  def changeLeaderIfNecessary(t: Term): Unit 

  def changeDefTermIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Unit 

  def updateCongrClassIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Unit 
}