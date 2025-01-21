package inca.ir.optimize

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.visitors.IRVisitor
import inca.util.{DEFAULT_PRINTER, printSteps}

trait BaseIROptimizer[V, RV, TV] extends IRVisitor with Optimizer:
  // Configure
  val computeControlEvents: Boolean
  val assumeEdbIsNotEmpty: Boolean

  val abstractInterpreter: BaseGenericInterpreter[V, ?, RV, ?, ?]

  def getTermResult(term: Term): Set[TV]

  def getBodyResult(body: Body): Set[RV]

  def getRelationResult(relation: Relation): Set[RV]

  var params: Set[Ref[Var.Target]] = Set()
  var boundBodyVars: Set[Ref[Var.Target]] = Set()

  def atomBindsRelevantVar(atom: Atom): Boolean =
    val boundVars = atom.vars.filter(_.mode.isBinding)
    boundVars.exists(bind => boundBodyVars.contains(bind.ref) || params.contains(bind.ref))

  // You need to enable computeControlEvents to get a control graph
  def controlGraph: Option[String] = None

  def analyzeProgram(modules: Seq[Module]): Unit =
    // Important, evaluate the program first
    abstractInterpreter.evalProgram(modules)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    params = relation.params.map(p => RefByName(p.name)).toSet
    super.visitRelation(relation)
  }

  /*override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    boundBodyVars = body.vars.filter(_.mode.isBound).map(_.ref).toSet
    super.visitBody(body)
  }*/
