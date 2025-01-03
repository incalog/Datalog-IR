package inca.ir.valueNumbering.VNTables

import inca.ir.{Body, Param, Relation, Term, string2name}



class VNTablesRelations(override val congrClasses: CongrClassesTable[Relation], override val valueNumbers: ValueIds[Relation])
  extends VNTablesTrait[Relation](congrClasses, valueNumbers){

  // name of relation has to be irrelevant for finding duplicate but has to be saved for replacement
  private def getLookupRelation(relation: Relation): Relation = Relation("Dummy", relation.params, relation.bodies) // TODO

  override val newCongrClass: (ValueId, Relation) => CongruenceClass[Relation] = {
    (valueId, relation) => CongruenceClassRelations(valueId, relation)
  }

  override def isValNumContained(relation: Relation): Boolean = {
    val lookupRelation = getLookupRelation(relation)
    super.isValNumContained(lookupRelation)
  }

  override def getIdOf(relation: Relation): ValueId = {
    val lookupRelation = getLookupRelation(relation)
    super.getIdOf(lookupRelation)
  }

  override def updateValNum(relation: Relation, vn: ValueId): Unit = {
    val lookupRelation = getLookupRelation(relation)
    super.updateValNum(lookupRelation,vn)
  }

  def addCongrClass(vn: ValueId, leader: Relation): Unit = addCongrClass(newCongrClass(vn, leader))

}


private object VNTablesRelations {
  def apply(): VNTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())
}
