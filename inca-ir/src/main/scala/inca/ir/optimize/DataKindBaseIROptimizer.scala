package inca.ir.optimize

import inca.ir
import inca.util.{Memoize, memoize}
import inca.ir.Hint.preserveHints
import inca.ir.analysis.{IRConstantAbstractInterpreter, IRDataKindAbstractInterpreter}
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.{Arg, Atom, Body, Call, Cast, Eq, ExtensionalRelation, ModuleEntry, Name, Param, Ref, RefByName, Relation, Term, TermArg, Type, Var, WildcardArg}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import inca.ir.extension.datamatch as irdatamatch
import inca.ir.extension.aggregate as iragg
import inca.ir.extension.bool as irbool
import inca.ir.extension.block as irblock
import inca.ir.extension.data.analysis.interpreter.DataKindV
import inca.ir.extension.tuple as irtuple
import inca.ir.extension.set as irset
import inca.ir.extension.map as irmap
import inca.ir.extension.disjunction as irdisjunction
import inca.ir.extension.tuple.analysis.{AbstractEdbConfig, EdbConfig}
import inca.ir.hints.MainHint
import sturdy.values.Topped

trait DataKindBaseIROptimizer(val interRelational: Boolean) extends BaseIROptimizer[Value, AbstractRelation, Value]:
  override def name: String =
    if (interRelational)
      "Data kind optimizer (inter)"
    else
      "Data kind optimizer (intra)"

  override val abstractInterpreter: IRDataKindAbstractInterpreter = new IRDataKindAbstractInterpreter(
    logControlEvents = computeControlEvents,
    interRelational = interRelational
  )

  override def controlGraph: Option[String] =
    if (computeControlEvents)
      val dotString = abstractInterpreter.graphBuilder.get.toGraphViz
      Some(s"digraph ControlGraph {$dotString\n}")
    else
      None

  val eqOps: BaseEqOps = abstractInterpreter.eqOps

  import abstractInterpreter.analysisAnnotator.{RelationKey, TermKey, BodyKey}

  override def getTermResult(term: Term): Set[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

  def getDataKinds(term: Term): Option[DataKindV] =
    getTermResult(term).headOption match
      case Some(v: DataKindV) => Some(v)
      case _ => None

  override def getBodyResult(body: Body): Set[AbstractRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[AbstractRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)

class IRDataKindOptimizer(
                           override val computeControlEvents: Boolean,
                           override val interRelational: Boolean = false,
                           override val edbConfig: EdbConfig[AbstractRelation] = AbstractEdbConfig.default
                         )
  extends DataKindBaseIROptimizer(interRelational)
  with irdatamatch.optimize.DataKindOptimizer



