package inca.ir.optimize

import inca.ir
import inca.util.memoize
import inca.ir.Hint.preserveHints
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ConstantRelation, Value, Top as TopV}
import inca.ir.{Atom, Body, Call, Cast, Eq, ExtensionalRelation, ModuleEntry, Relation, Term}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import inca.ir.extension.aggregate as iragg
import inca.ir.visitors.BaseIRVisitor
import inca.util.printStep
import sturdy.values.Topped

extension [T](topped: Topped[T])
  def isTrue: Boolean = topped.isActual && topped.get == true
  def isFalse: Boolean = topped.isActual && topped.get == false

trait ConstantBaseIROptimizer(val interRelational: Boolean) extends BaseIROptimizer[Value, ConstantRelation, Value]:
  override def name: String = "Constant Optimizer"

  override val abstractInterpreter: IRConstantAbstractInterpreter = new IRConstantAbstractInterpreter(
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

  override def getBodyResult(body: Body): Set[ConstantRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[ConstantRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  private def relationAlwaysFails(relation: Relation): Boolean =
    getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }

  private def bodyAlwaysFails(body: Body): Boolean =
    getBodyResult(body).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }

  /*private def relationAlwaysSucceeds(relation: Relation): Boolean =
    getRelationResult(relation)
      .map(_.empty).forall {
        case Topped.Actual(v) => !v
        case Topped.Top => false
      }*/

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
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

    super.analyzeProgram(modules)

  // Override the internal method in the children
  private lazy val valueToTerm = memoize(valueToTermInternal)
  def valueToTermInternal(value: Value): Option[Term] =
    None

  private def transformTerm(term: Term): Option[Term] =
    getTermResult(term).headOption.flatMap(valueToTerm.apply)

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    // Remove empty relations. We know that there can not be any call site for these relations, because a failing
    // call will lead to a failing body at the call site. Except if the call is a negative call, in which case it
    // always succeeds.
    if (relationAlwaysFails(relation))
      Seq()
    else
      super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    if (bodyAlwaysFails(body))
      Seq()
    else
      super.visitBody(body)
  }

  protected def binCompare(lhs: Term, rhs: Term, op: (Value, Value) => Boolean): Boolean =
    (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
      case (Some(v1), Some(v2)) => op(v1, v2)
      case _ => false

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
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
      // Only relevant for intra-relation analysis, inter-relational analysis should detect this
      case Call(ref, args, false) =>
        ref.target match
          case Some(r: Relation) if relationAlwaysFails(r) => throw FailedBody
          case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  // Replace all terms with their constants if possible
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    if (!term.typ.get.mode.isBinding)
      val transformed = term match
        case Cast(t, ty) => transformTerm(term).map(Cast(_, ty))
        case _ => transformTerm(term).map(Cast(_, term.typ.get.ty))
      transformed match
        case Some(newTerm) => Seq(newTerm)
        case _ => super.visitTerm(term)
    else
      super.visitTerm(term)
  }

class IRConstantOptimizer(
       override val assumeEdbIsNotEmpty: Boolean,
       override val computeControlEvents: Boolean,
       override val interRelational: Boolean = false
  )
  extends ConstantBaseIROptimizer(interRelational)
  with irarith.optimize.ConstantOptimizer
  with irstr.optimize.ConstantOptimizer
  with irdata.optimize.ConstantOptimizer
  with iragg.optimize.ConstantOptimizer



