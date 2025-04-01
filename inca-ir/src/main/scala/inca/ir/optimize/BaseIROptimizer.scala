package inca.ir.optimize

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.analysis.base.interpreter.BaseGenericInterpreter
import inca.ir.printer.IRDebugPrinter
import inca.ir.visitors.IRVisitor
import sturdy.effect.failure.AFallible
import sturdy.values.Topped

case class AnalysisFailed(msg: String) extends Exception:
  override def toString: String = msg

extension [T](topped: Topped[T])
  def isTrue: Boolean = topped.isActual && topped.get == true
  def isFalse: Boolean = topped.isActual && topped.get == false

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
    val analysisRes = abstractInterpreter.failure.fallible {
      abstractInterpreter.evalProgram(modules)
    }
    analysisRes match {
      case AFallible.Failing(failures) =>
        val msg = failures.map { (kind, message) =>
          s"[$kind]: $message"
        }.set.mkString("\n")
        throw AnalysisFailed(msg)
      case AFallible.Diverging(recur) =>
        throw IllegalStateException()
        // TODO: unhandled error, occurs for FixFunction compiler test if main is
        //  executed before the extensional call
      case _ => // nothing
    }

    println(new IRDebugPrinter{}.prettyPrint(modules))

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module]): Seq[Module] =
    if (!analysisHasRun)
      analyzeProgram(modules)
    super.visitProgram(modules, dependencies)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    params = relation.params.map(p => RefByName(p.name) -> p.ty).toMap
    super.visitRelation(relation)
  }
