package inca.ir.optimize

import inca.ir
import inca.util.{Memoize, memoize}
import inca.ir.Hint.preserveHints
import inca.ir.analysis.IROODLClassAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.{Arg, Atom, Body, Call, Cast, Eq, ExtensionalRelation, MainHint, ModuleEntry, Name, Param, Ref, RefByName, Relation, Term, TermArg, Type, Var, WildcardArg}
import inca.ir.extension.data as irdata
import inca.ir.extension.data.analysis.interpreter.OODLClassV
import sturdy.values.Topped

trait OODLClassBaseIROptimizer(val _superClassMap: Map[String, Set[String]], val _interRelational: Boolean)
  extends BaseIROptimizer[Value, AbstractRelation, Value]:

  override def name: String =
    if (_interRelational)
      "OODL class optimizer (inter)"
    else
      "OODL class optimizer (intra)"

  override val abstractInterpreter: IROODLClassAbstractInterpreter = new IROODLClassAbstractInterpreter(
    superClassMap = _superClassMap,
    logControlEvents = computeControlEvents,
    interRelational = _interRelational
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

  def getDataKinds(term: Term): Option[OODLClassV] =
    getTermResult(term).headOption match
      case Some(v: OODLClassV) => Some(v)
      case _ => None

  override def getBodyResult(body: Body): Set[AbstractRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[AbstractRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, args) =
            if (n.name == "ext_main$input")
              params.map {
                case p if p.name.name == "Alloc" => (p.name.name, ConstantIntV(1))
                case p if p.name.name == "Mutation" => (p.name.name, ConstantIntV(1))
                case p if p.name.name == "MonoImpurity" => (p.name.name, ConstantIntV(1))
                case p => (p.name.name, Value.Top)
              }.unzip
            else
              params.map(p => (p.name.name, Value.Top)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, AbstractRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)

class IROODLClassOptimizer(
                            val superClassMap: Map[String, Set[String]],
                            override val assumeEdbIsNotEmpty: Boolean,
                            override val computeControlEvents: Boolean,
                            val interRelational: Boolean = false
                         )
  extends OODLClassBaseIROptimizer(superClassMap, interRelational)
  with irdata.optimize.OODLClassOptimizer



