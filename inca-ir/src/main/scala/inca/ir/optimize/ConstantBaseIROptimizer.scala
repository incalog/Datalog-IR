package inca.ir.optimize

import inca.ir
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ConstantRelation, Value, Top as TopV}
import inca.ir.{Atom, Body, Call, Cast, Eq, ExtensionalRelation, Relation, Term}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import sturdy.values.Topped

//import java.awt.Toolkit
//import java.awt.datatransfer.StringSelection

extension [T](topped: Topped[T])
  def isTrue: Boolean = topped.isActual && topped.get == true
  def isFalse: Boolean = topped.isActual && topped.get == false

trait ConstantBaseIROptimizer extends BaseIROptimizer[Value, ConstantRelation, Value]:
  override def name: String = "Constant Optimizer"

  override val abstractInterpreter: IRConstantAbstractInterpreter = new IRConstantAbstractInterpreter()

  val eqOps: BaseEqOps = abstractInterpreter.eqOps

  import abstractInterpreter.analysisAnnotator.{RelationKey, TermKey, BodyKey}

  val assumeEdbIsNotEmpty: Boolean

  override def getTermResult(term: Term): Set[Value] =
    term.getAnalysisResult(TermKey).map(_.value)

  override def getBodyResult(body: Body): Set[ConstantRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[ConstantRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  private def relationAlwaysFails(relation: Relation): Boolean =
    getRelationResult(relation).map(_.empty).forall {
        case Topped.Actual(v) => v
        case Topped.Top => false
      }

  private def relationAlwaysSucceeds(relation: Relation): Boolean =
    getRelationResult(relation)
      .map(_.empty).forall {
        case Topped.Actual(v) => !v
        case Topped.Top => false
      }

  override def visitProgram(modules: Seq[ir.Module], dependencies: Seq[ir.Module]): Seq[ir.Module] =
    // We could make this more precise, by setting the `empty` flag correctly on edb relations
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, args) = params.map(p => (p.name.name, TopV)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, ConstantRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }

    // Analyse the program
    evalProgram(modules)

    // Repeat multiple times
    /*val r = 0.until(2).foldLeft(modules) { (mods, _) =>
      val m = super.visitProgram(mods, dependencies)
      println(m)
      m
    }*/

    val r = super.visitProgram(modules, dependencies)

    /*val stringSelection = new StringSelection(s"digraph G {${abstractInterpreter.graphBuilder.get.toGraphViz}\n}")
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(stringSelection, null)*/

    r

  private var valueCache: Map[Value, Term] = Map()

  // Override this in a child
  def valueToTerm(value: Value): Option[Term] =
    None

  private def transformTerm(term: Term): Option[Term] =
    val annotatedValue = getTermResult(term).headOption
    val cachedTerm = annotatedValue.flatMap(valueCache.get)
    (annotatedValue, cachedTerm) match
      case (_, Some(transformedTerm)) => Some(transformedTerm)
      case (Some(value), _) => valueToTerm(value) match
        case Some(newTerm) =>
          valueCache += value -> newTerm
          Some(newTerm)
        case None => None
      case _ => None

  override def visitRelation(relation: Relation): Seq[Relation] =
    // Remove empty relations. We know that there can not be any call site for these relations, because a failing
    // call will lead to a failing body at the call site. Except if the call is a negative call, in which case it
    // always succeeds.
    if (relationAlwaysFails(relation))
      Seq()
    else
      super.visitRelation(relation)

  override def visitBody(body: Body): Seq[Body] =
    getBodyResult(body).headOption match
      case Some(res: ConstantRelation) if res.empty.isTrue =>
        // Remove failing bodies
        Seq()
      case _ => super.visitBody(body)

  protected def binCompare(lhs: Term, rhs: Term, op: (Value, Value) => Boolean): Boolean =
    (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
      case (Some(v1), Some(v2)) => op(v1, v2)
      case _ => false

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    // Remove equality constraints that always hold
    case Eq(lhs, rhs, neg) if !atomBindsRelevantVar(atom) =>
      val op = if (neg) eqOps.neq else eqOps.equ
      if (binCompare(lhs, rhs, op(_, _).isTrue))
        Seq()
      else
        super.visitAtom(atom)
    // A negative call to a failing relation always succeeds
    case Call(ref, args, true) =>
      ref.target match
        case Some(r: Relation) if relationAlwaysFails(r) => Seq()
        case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

  // Replace all terms with their constants if possible
  override def visitTerm(term: Term): Seq[Term] =
    if (!term.typ.get.mode.isBinding)
      val transformed = term match
        case Cast(t, ty) => transformTerm(term).map(Cast(_, ty))
        case _ => transformTerm(term)
      transformed match
        case Some(newTerm) => Seq(newTerm)
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)

class IRConstantOptimizer(override val assumeEdbIsNotEmpty: Boolean)
  extends ConstantBaseIROptimizer
  with irarith.optimize.ConstantIROptimizer
  with irstr.optimize.ConstantIROptimizer
  with irdata.optimize.ConstantIROptimizer



