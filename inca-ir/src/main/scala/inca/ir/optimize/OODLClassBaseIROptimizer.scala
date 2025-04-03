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
import inca.ir.extension.string as irstring
import inca.ir.extension.data.analysis.interpreter.OODLClassV
import sturdy.values.Topped

import scala.compiletime.uninitialized

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

  lazy val ancestors: Map[String, Set[String]] = abstractInterpreter.ancestors
  lazy val descendants: Map[String, Set[String]] = abstractInterpreter.descendants
  val classes: Set[String] = _superClassMap.keySet + "Object" + "Null"

  override def getTermResult(term: Term): Set[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

  def getClass(term: Term): Option[OODLClassV] =
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
          val aRel = edbConfig.abstractExtensionalRelation(n, params)
          abstractInterpreter.insertEDB(n.name, aRel)
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)


  /*private var variableRemapping: Map[Name, Term] = uninitialized

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(p => RefByName(p.name) -> p.ty).toMap
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap { b =>
      variableRemapping = Map()
      visitBody(b)
    }))*/

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match
      // runtimeType$ is a reserved OODL method. All it does is destructing an
      // OID to get the contained class.
      // If we know the precise class, we can eliminate the whole call.
      case Call(ref, args, neg) if ref.name.name == "runtimeType$" =>
        assert(args.size == 2)
        (args.head, args.last) match
          case (TermArg(oid), TermArg(clsTerm)) =>
            getClass(oid) match
              case Some(OODLClassV(cls, true)) =>
                logOptimizationStat("constant runtimeType$", 1,_+1)
                //variableRemapping += clsVar.ref.name -> irstring.StringLit(cls)
                Seq(Eq(clsTerm, irstring.StringLit(cls)))
              case _ => super.visitAtom(atom)
          case _ => super.visitAtom(atom)

        
      case _ => super.visitAtom(atom)

  /*override def visitTerm(term: Term): Seq[Term] =
    term match
      case Var(ref) =>
        Seq(variableRemapping.getOrElse(ref.name, term))
      case _ =>
        super.visitTerm(term)*/


class IROODLClassOptimizer(
                            val superClassMap: Map[String, Set[String]],
                            override val computeControlEvents: Boolean,
                            val interRelational: Boolean = false,
                            override val edbConfig: EdbConfig[AbstractRelation] = OODLEdbConfig.default
                         )
  extends OODLClassBaseIROptimizer(superClassMap, interRelational)
  with irdata.optimize.OODLClassOptimizer



