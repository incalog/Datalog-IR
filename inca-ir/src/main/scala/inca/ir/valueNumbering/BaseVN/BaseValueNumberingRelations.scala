package inca.ir.valueNumbering.BaseVN

import inca.ir
import inca.ir.*
import inca.ir.typing.IRTypechecker
import inca.ir.valueNumbering.VNTables.*
import inca.ir.visitors.IRVisitor



trait BaseValueNumberingRelations extends BaseValueNumberingBodies {

  private var vnTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())

  private var oldVNTablesRelations: VNTablesRelations = vnTablesRelations // saved for replacement in repetition phase

  private var removedRelation: Boolean = false


  private class updateRefsToRemovedRelation extends IRVisitor {
    // otherwise it can happen that a relation was removed but a call not renamed if the call stands before the relation
    // -> typechecker in repetition phase throws error
    override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref.target match {
      case Some(rel : Relation) if relations.contains(ref.name.name) =>
        val relation = relations(ref.name.name)
        // in repetition phase it might happen that relation not in congrClass anymore -> used tables of previous iteration
        val newName = vnTablesRelations.getReplacement(relation).name
        val newRef = RefByName[Relation](newName)
        newRef.target = Some(rel)
        newRef.asInstanceOf[Ref[Target]]
      case _ => ref
    }
  }


  def printResultsRelations(): Unit = {
    if (!printVNResults) return
    println(s"Results from VN of Relations after iteration $currentIteration")
    vnTablesRelations.printResults()
  }


  override private[BaseVN] def repetitionPhase(module: Module): Module = {
    removedRelation = false
    oldVNTablesRelations = vnTablesRelations
    vnTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())
    printResultsRelations()

    var result = super.visit(module)
    if (removedRelation) {
      result = new updateRefsToRemovedRelation().visitModule(result)
    }
    val typechecker = new IRTypechecker {}
    typechecker.checkProgram(Seq(result))
    if (result != module && useFixPointIteration) {
      return repetitionPhase(result)
    }
    else {
      return result
    }
  }


  override def visitRelation(relation: Relation): Seq[Relation] = {
    val newRelation = super.visitRelation(relation)
    valueNumberRelations(newRelation)
  }


  protected def normalizeRelation(relation: Relation): Seq[Relation] = Seq(relation) // TODO


  private def valueNumberRelations(relationSeq: Seq[Relation]): Seq[Relation] = {
    if (relationSeq.isEmpty) return relationSeq
    val relation = normalizeRelation(relationSeq.head) match {
      case h :: _ => h
      case _ => return Seq()
    }

    // make sure that rewritten relation and old relation are equal (i.e. get same value number)
    if (vnTablesRelations.isValNumContained(oldRelation)) {
      val oldVN = vnTablesRelations.getIdOf(oldRelation)
      vnTablesRelations.updateValueNumbersAndCongrClasses(relation,oldVN)
    }

    val vn: ValueId = vnTablesRelations.getIdOf(relation)

    if (vnTablesRelations.isCongrClassContained(vn)) {
      removedRelation = true // then it might be necessary to rename references to the relation
      return Seq()
    }
    else {
      vnTablesRelations.addCongrClass(vn, relation)
      return Seq(relation)
    }
  }


  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref.target match {
    case Some(rel : Relation) if relations.contains(ref.name.name) =>
      val relation = relations(ref.name.name)
      // in repetition phase it might happen that relation not in congrClass anymore -> used tables of previous iteration
      val newName = oldVNTablesRelations.getReplacement(relation).name
      val newRef = RefByName[Relation](newName)
      newRef.target = Some(rel)
      newRef.asInstanceOf[Ref[Target]]
    case _ => ref
  }


}