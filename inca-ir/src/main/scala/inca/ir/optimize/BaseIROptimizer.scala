package inca.ir.optimize

import inca.ir.*
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.visitors.IRVisitor
import inca.util.printStep

trait BaseIROptimizer[V, RV, TV] extends IRVisitor:
  val abstractInterpreter: BaseGenericInterpreter[V, ?, RV, ?, ?]
  var logAnalysis: Boolean = false
  
  def getTermResult(term: Term): Set[TV]

  def getBodyResult(body: Body): Set[RV]

  def getRelationResult(relation: Relation): Set[RV]

  var params: Set[Ref[Var.Target]] = Set()

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module]): Seq[Module] =
    // Important, evaluate the program first
    abstractInterpreter.evalProgram(modules)
    if (logAnalysis)
      printStep(s"Analysis: $name", modules)
    super.visitProgram(modules, dependencies)

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(p => RefByName(p.name)).toSet
    super.visitRelation(relation)

  var boundBodyVars: Set[Ref[Var.Target]] = Set()

  override def visitBody(body: Body): Seq[Body] =
    boundBodyVars = body.vars.filter(_.mode.isBound).map(_.ref).toSet
    super.visitBody(body)

  private def atomBindsRelevantVar(atom: Atom): Boolean =
    val boundVars = atom.vars.filter(_.mode.isBinding)
    boundVars.exists(bind => boundBodyVars.contains(bind.ref) || params.contains(bind.ref))

