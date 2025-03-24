package inca.ir.optimize

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.printer.IRDebugPrinter
import inca.ir.visitors.IRVisitor
import inca.util.{DEFAULT_PRINTER, printSteps}
import sturdy.effect.failure.{AFallible, CollectedFailures}

trait BaseIROptimizer[V, RV, TV] extends IRVisitor with Optimizer:
  // Configure
  val computeControlEvents: Boolean
  val assumeEdbIsNotEmpty: Boolean

  val abstractInterpreter: BaseGenericInterpreter[V, ?, RV, ?, ?]

  def getTermResult(term: Term): Set[TV]

  def getBodyResult(body: Body): Set[RV]

  def getRelationResult(relation: Relation): Set[RV]

  var params: Map[Ref[Var.Target], Type] = Map()

  def isParam(ref: Ref[Var.Target]): Boolean =
    params.contains(ref)

  // You need to enable computeControlEvents to get a control graph
  def controlGraph: Option[String] = None

  private var analysisHasRun: Boolean = false
  /** Important, evaluate the program first */
  override def analyzeProgram(modules: Seq[Module]): Unit =
    analysisHasRun = true
    abstractInterpreter.failure.fallible {
      abstractInterpreter.evalProgram(modules)
    }.get

    println(new IRDebugPrinter{}.prettyPrint(modules))

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module]): Seq[Module] =
    if (!analysisHasRun)
      analyzeProgram(modules)
    super.visitProgram(modules, dependencies)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    params = relation.params.map(p => RefByName(p.name) -> p.ty).toMap
    super.visitRelation(relation)
  }
