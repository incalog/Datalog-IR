package inca.ir.valueNumbering.VNTables

import inca.ir.{Body, Param, Relation, Term, string2name}



class VNTablesRelations(override val congrClasses: CongrClassesTable[Relation], override val valueNumbers: ValueIds[Relation])
  extends VNTablesTrait[Relation](congrClasses, valueNumbers){

  // name of relation has to be irrelevant for finding duplicate but has to be saved for replacement;
  // name "Dummy" is only used internally and not propagated out of this class
  private def getLookupRelation(relation: Relation): Relation = Relation("Dummy", relation.params, relation.bodies)

  override val newCongrClass: (ValueNumber, Relation) => CongruenceClass[Relation] = {
    (valueNumber, relation) => CongruenceClassRelations(valueNumber, relation)
  }

  override def isValNumContained(relation: Relation): Boolean = {
    val lookupRelation = getLookupRelation(relation)
    super.isValNumContained(lookupRelation)
  }

  override def getValNumOf(relation: Relation): ValueNumber = {
    val lookupRelation = getLookupRelation(relation)
    super.getValNumOf(lookupRelation)
  }

  override def updateValNum(relation: Relation, vn: ValueNumber): Unit = {
    val lookupRelation = getLookupRelation(relation)
    super.updateValNum(lookupRelation,vn)
  }

}


private object VNTablesRelations {
  def apply(): VNTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())
}
