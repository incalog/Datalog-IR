package inca.ir.optimize

import inca.ir
import inca.util.{Gensym, Memoize, memoize}
import inca.ir.Hint.preserveHints
import inca.ir.analysis.IROODLClassAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.{Arg, Atom, Body, Call, Cast, Eq, ExtensionalRelation, ModuleEntry, Name, Param, Ref, RefByName, Relation, Term, TermArg, Type, Var, WildcardArg}
import inca.ir.extension.data as irdata
import inca.ir.extension.string as irstring
import inca.ir.extension.data.analysis.interpreter.OODLClassV
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.analysis.{AbstractEdbConfig, EdbConfig}
import inca.ir.hints.MainHint
import inca.ir.visitors.IRVisitor
import sturdy.values.Topped

import scala.compiletime.uninitialized

class OODLEdbConfig extends AbstractEdbConfig:
  override def abstractExtensionalRelation(n: Name, params: Seq[Param]): AbstractRelation =
    if (n.name == "ext_main$input")
      // OODL specific
      val (aCols, aRows) = params.map {
        case p if p.name.name == "Alloc" => (p.name.name, ConstantIntV(1))
        case p if p.name.name == "Mutation" => (p.name.name, ConstantIntV(1))
        case p if p.name.name == "MonoImpurity" => (p.name.name, ConstantIntV(1))
        case p => (p.name.name, Value.Top)
      }.unzip
      AbstractRelation(aCols, aRows, Topped.Actual(false))
    else
      super.abstractExtensionalRelation(n, params)

object OODLEdbConfig:
  val default: OODLEdbConfig = new OODLEdbConfig


// TODO: Move this to the OODL package
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

  private def getDispatchMapping(dispatchRel: Relation) =
    memoize(getDispatchMappingInternal(dispatchRel))

  private def getDispatchMappingInternal(dispatchRel: Relation): Map[String, String] =
    // Dispatch bodies are always mutually exclusive. They can not fail, and we
    // Should always get precise constant information of the bodies.
    dispatchRel.bodies.flatMap { b =>
      getBodyResult(b).headOption match
        case Some(AbstractRelation.NonEmpty(cols, rows, _)) =>
          // OODL specific column names
          val srcCol = cols.indexOf("src")
          val trgCol = cols.indexOf("trg")
          val rowCls = rows.map { case ConstantStringV(cls) => cls }
          Some(rowCls(srcCol) -> rowCls(trgCol))
        case Some(_) =>
          throw IllegalStateException("Unexpected body result for dispatch relation")
        case _ =>
          // Body was not reachable, thus no annotation exists.
          None
    }.toMap

  private def lookupDispatchTarget(dispatchRel: Relation, src: String): String =
    val dispatchTable = getDispatchMapping(dispatchRel)
    dispatchTable(src)

  // TODO: I think it makes more sense to
  //  1. Do a constant propagation
  //  2. Specialize relations syntactically
  // Method Name -> Name of classes for which it makes sense to specialize the method for
  /*private var specializeRelationsMapping: Map[String, Set[String]] = uninitialized

  private var relGensym: Gensym = uninitialized

  private def specializeRelation(rel: Relation, cls: String): (Relation, Option[Relation]) =
    val matchingBody: Option[Body] = rel.bodies.find { b =>
      getBodyResult(b).headOption.exists {
        case AbstractRelation.NonEmpty(cols, rows, _) =>
          rows.headOption match
            case Some(ConstantStringV(s)) => s == cls
            case _ => false
        case rv => false
      }
    }
    matchingBody match
      case Some(body) =>
        val newRelName = relGensym.freshName(rel.name)
        val newRel = Relation(newRelName, rel.params, Seq(body))
        val originalRel = preserveHints(rel) {
          Relation(rel.name, rel.params, rel.bodies.map { b =>
            if (b == body)
              Body(Seq(Call(newRelName, rel.params.map(p => Var(p.name).arg))))
            else
              b
          })
        }
        (originalRel, Some(newRel))
      case _ =>
        (rel, None)


  override def visitModule(module: ir.Module): ir.Module =
    specializeRelationsMapping = Map()
    relGensym = new Gensym()

    val mod@ir.Module(name, lang, contents) = super.visitModule(module)
    relGensym.register(mod.relations.keys)

    val newContent = contents.flatMap {
      case rel: Relation =>
        specializeRelationsMapping.get(rel.name.name) match
          case Some(clsNames) =>
            val (orgUpdated, newRels) = clsNames.foldLeft((rel, Seq[Relation]())) { case ((prevRel, newRels), cls) =>
              val (orgRel, newRel) = specializeRelation(prevRel, cls)
              (orgRel, newRels ++ newRel)
            }
            orgUpdated +: newRels
          case _ => Seq(rel)
      case c => Seq(c)
    }
    // TODO: Specialize calls
    ir.Module(name, lang, newContent)*/

  private var runtimeTypeMapping: Map[Term, String] = uninitialized
  private var dispatchMapping: Map[Term, String] = uninitialized

  override def visitRelation(relation: Relation): Seq[Relation] =
    params = relation.params.map(p => RefByName(p.name) -> p.ty).toMap
    Seq(Relation(relation.name, relation.params.flatMap(visitParam), relation.bodies.flatMap { b =>
      runtimeTypeMapping = Map()
      dispatchMapping = Map()
      visitBody(b)
    }))

  override def visitAtom(atom: Atom): Seq[Atom] =
    atom match

      // runtimeType$ is a reserved OODL method. It destructs an OID to get
      // the contained runtime class. Calls to this method can not fail.
      // If we know the precise class, we can eliminate the whole call.
      case Call(ref, args, false) if ref.name.name == "runtimeType$" && args.size == 2 =>
        (args.head, args.last) match
          case (TermArg(oid), TermArg(clsTerm)) =>
            getClass(oid) match
              case Some(OODLClassV(cls, true)) =>
                logOptimizationStat("constant runtimeType$", 1,_+1)
                runtimeTypeMapping += clsTerm -> cls
                Seq(Eq(clsTerm, irstring.StringLit(cls)))
              case _ => super.visitAtom(atom)
          case _ => super.visitAtom(atom)

      // Dispatch calls are reserved OODL methods. They dynamically look up which
      // implementation of a method to call based on the runtime type.
      // If we know the runtime type we can figure out the dispatch target.
      // Calls to this method can not fail.
      case Call(ref, args, false) if ref.name.name.startsWith("dispatch$") && args.size == 2 =>
        val rel = ref.target.get match
          case r: Relation => r
          case _ => throw IllegalStateException("Expected dispatch idb relation!")
        (args.head, args.last) match
          case (TermArg(srcTerm), TermArg(trgTerm)) =>
            val srcClsOption = getTermResult(srcTerm).headOption match
              case Some(ConstantStringV(s)) => Some(s)
              case _ =>
                // to prevent a second pass of this optimization
                // Note: This relies on the order, that means runtimeType$
                // needs to be called before dispatch$
                runtimeTypeMapping.get(srcTerm)
            srcClsOption match
              case Some(srcCls) =>
                logOptimizationStat("constant dispatch$", 1,_+1)
                val trgCls = lookupDispatchTarget(rel, srcCls)
                dispatchMapping += trgTerm -> trgCls
                val companionRelName = ref.name.name.stripPrefix("dispatch$")
                //specializeRelationsMapping += companionRelName -> (specializeRelationsMapping.getOrElse(companionRelName, Set()) + trgCls)
                Seq(Eq(trgTerm, irstring.StringLit(trgCls)))
              case _ => super.visitAtom(atom)
          case _ => super.visitAtom(atom)

      // The subtype relation holds if the first argument is a subtype of the second.
      // If we statically know the subtype, we can optimize them away or decide if the
      // call fails.
      case Call(ref, args, neg) if ref.name.name == "subtype$" && args.size == 2 =>
        val rel = ref.target.get match
          case r: Relation => r
          case _ => throw IllegalStateException("Expected dispatch idb relation!")
        (args.head, args.last) match
          case (TermArg(subTerm), TermArg(superTerm)) =>
            val subClsOption = getTermResult(subTerm).headOption match
              case Some(ConstantStringV(sub)) => Some(sub)
              case _ => runtimeTypeMapping.get(subTerm)
            val superClsOption = getTermResult(superTerm).headOption match
              case Some(ConstantStringV(sup)) => Some(sup)
              case _ => runtimeTypeMapping.get(superTerm)
            (subClsOption, superClsOption) match
              case (Some(subCls), Some(superCls)) =>
                logOptimizationStat("constant subtype", 1,_+1)
                val isSubtype = (subCls == superCls) || ancestors.getOrElse(subCls, Set()).contains(superCls)
                if ((neg && !isSubtype) || (!neg && isSubtype))
                  Seq()
                else
                  throw FailedBody
              case _ => super.visitAtom(atom)
          case _ => super.visitAtom(atom)

      case _ =>
        super.visitAtom(atom)

class IROODLClassOptimizer(
                            val superClassMap: Map[String, Set[String]],
                            override val computeControlEvents: Boolean,
                            val interRelational: Boolean = false,
                            override val edbConfig: EdbConfig[AbstractRelation] = OODLEdbConfig.default
                         )
  extends OODLClassBaseIROptimizer(superClassMap, interRelational)
  with irdata.optimize.OODLClassOptimizer



