package inca.ir.optimize

import inca.ir
import inca.util.{Memoize, memoize}
import inca.ir.Hint.preserveHints
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.{Arg, Atom, Body, Call, Cast, Eq, ExtensionalRelation, MainHint, ModuleEntry, Name, Param, Ref, RefByName, Relation, Term, TermArg, Type, Var, WildcardArg}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import inca.ir.extension.aggregate as iragg
import sturdy.values.Topped

extension [T](topped: Topped[T])
  def isTrue: Boolean = topped.isActual && topped.get == true
  def isFalse: Boolean = topped.isActual && topped.get == false

trait ConstantBaseIROptimizer(val interRelational: Boolean) extends BaseIROptimizer[Value, AbstractRelation, Value]:
  override def name: String =
    if (interRelational)
      "Constant optimizer (inter)"
    else
      "Constant optimizer (intra)"

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

  override def getBodyResult(body: Body): Set[AbstractRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[AbstractRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  protected def relationAlwaysFails(relation: Relation): Boolean =
    getRelationResult(relation).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }

  def relationUsedInAggregation(relation: Relation): Boolean =
    false

  private def bodyAlwaysFails(body: Body): Boolean =
    getBodyResult(body).map(_.empty).forall {
      case Topped.Actual(v) => v
      case Topped.Top => false
    }

  override def analyzeProgram(modules: Seq[ir.Module]): Unit =
    modules.foreach { m =>
      m.entries.foreach {
        case (_, ExtensionalRelation(n, params)) =>
          val (paramNames, args) = params.map(p => (p.name.name, Value.Top)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, AbstractRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)

  lazy val valueToTerm: Memoize[Value, Option[Term]] = memoize(valueToTermInternal)
  // Override this internal method in subclasses
  def valueToTermInternal(value: Value): Option[Term] = None

  private def transformTerm(term: Term): Option[Term] =
    val option = getTermResult(term).headOption
    option.flatMap(valueToTerm.apply) match
      case Some(value) => if (value == term) None else Some(value)
      case None => None

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    // Remove empty relations. We know that there can not be any call site for these relations, because a failing
    // call will lead to a failing body at the call site. Except if the call is a negative call, in which case it
    // always succeeds.
    if (relationAlwaysFails(relation)) {
      if (relationUsedInAggregation(relation))
        logOptimizationStat("aggregate empty relation", 1, _+1)
        Seq(relation.copy(bodies = Seq()))
      else
        logOptimizationStat("constant failed relation", 1, _+1)
        Seq()
    } else if (!relation.hasHint(MainHint)) {
      val res = getRelationResult(relation).headOption.get
      val nonconstantParams = relation.params.zip(res.rows).flatMap {
        case (p, v) if v.isConstant && mayEliminate(p)(relation) => None
        case (p, _) => Some(p)
      }
      if (nonconstantParams.isEmpty) {
        if (relationUsedInAggregation(relation))
          logOptimizationStat("aggregate empty relation", 1, _+1)
          Seq(relation.copy(bodies = Seq()))
        else
          logOptimizationStat("constant relation", 1, _+1)
          Seq()
      } else {
        val k = relation.params.size - nonconstantParams.size
        if (k != 0)
          logOptimizationStat("constant relation parameter", k, _ + k)
        super.visitRelation(relation.copy(params = nonconstantParams))
      }
    } else {
      super.visitRelation(relation)
    }
  }

  private def eqsToBindConstantParams(body: Body): Seq[Eq] =
    getBodyResult(body).headOption match
      case None => Seq()
      case Some(constRel) =>
        constRel.cols.zip(constRel.rows).flatMap { (c, v) =>
          valueToTerm(v).flatMap { t =>
            val expectedTy = params.get(RefByName(Name(c)))
            expectedTy.map(ty => Eq(Var(Name(c)), Cast(t, ty)))
          }
        }

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
    if (bodyAlwaysFails(body)) {
      logOptimizationStat("constant failed body", 1, _+1)
      Seq()
    } else {
      val eqAts = eqsToBindConstantParams(body)
      logOptimizationStat("constant equation", 1, _ - eqAts.size)
      super.visitBody(body)
        .map(b => Body(eqAts ++ b.atoms)) // .diff(b.atoms)
        .filter(_.atoms.nonEmpty)
    }
  }

  protected def binCompare[A](lhs: Term, rhs: Term, op: (Value, Value) => A): Option[A] =
    (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
      case (Some(v1), Some(v2)) => Some(op(v1, v2))
      case _ => None

  protected def mayEliminate(p: Param)(implicit relation: Relation): Boolean =
    true

  protected def mayEliminate(t: Term): Boolean =
    // See branching test. Our annotation is too imprecise, we work around this with a simple heuristic.
    // A term can only be constant, if we know that all variables used in the term are constant as well.
    isConstant(t) && t.vars.forall(isConstant)

  protected def extractBindingVarRef(arg: Arg): Option[Ref[Var.Target]] = arg match
    case TermArg(v@Var(ref)) if v.typ.get.mode.isBinding => Some(v.ref)
    case _ => None

  protected def argTy(arg: Arg): Type = arg match
    case TermArg(t) => t.typ.get.ty
    case w@WildcardArg() => w.typ.get.ty
  
  /*protected def mayEliminate(arg: Arg): Boolean = arg match
    case TermArg(t) => mayEliminate(t)
    case WildcardArg() => false*/
  
  protected def mayEliminate(eq: Eq): Boolean =
    eq.neg || (mayEliminate(eq.lhs) && mayEliminate(eq.rhs))

  protected def isConstant(arg: Arg): Boolean = arg match
    case TermArg(t) => isConstant(t)
    case WildcardArg() => false

  protected def isConstant(term: Term): Boolean =
    getTermResult(term).headOption.exists(_.isConstant)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case eq@Eq(lhs, rhs, neg) if mayEliminate(eq) =>
        val op = if (neg) eqOps.neq else eqOps.equ
        val comp = binCompare(lhs, rhs, op(_, _))
        val compAlwaysSucceeds = comp.exists(_.isTrue)
        val assignsConstant = isConstant(eq.lhs) || isConstant(eq.rhs)

        if (compAlwaysSucceeds || (eq.isAssignment && assignsConstant)) {
          logOptimizationStat("constant equation", 1, _ + 1)
          Seq()
        } else {
          super.visitAtom(atom)
        }
      // A negative call to a failing relation always succeeds
      case Call(ref, args, true) =>
        ref.target match
          case Some(r: Relation) if relationAlwaysFails(r) =>
            logOptimizationStat("constant failed neg-call", 1, _+1)
            Seq()
          case _ => super.visitAtom(atom)
      case call@Call(ref, args, false) =>
        ref.target match
          case Some(r: Relation) if relationAlwaysFails(r) =>
            logOptimizationStat("constant failed call", 1, _+1)
            throw FailedBody
          case Some(r: Relation) =>
            val res = getRelationResult(r).headOption.get
            val eliminatetable = r.params.map(mayEliminate(_)(r))
            val (constantArgs, nonconstantArgs) = args.zip(res.rows).zip(eliminatetable)
              .partition { case ((arg, res), elim) => res.isConstant && elim }
            val ats =
              if (nonconstantArgs.isEmpty)
                Seq()
              else
                Seq(call.copy(args = nonconstantArgs.flatMap { case ((a, _), _) => visitArg(a) }))

            // In case we have removed an argument that was binding a parameter or a variable required in the body, we 
            // need to insert an equality constraint for that variable.
            // Test (general problem): Datalog frontend -> lecture 5 -> nat relation
            // Test (why cast is needed): OODL -> Unit Test -> Subtyping
            constantArgs.flatMap { case ((a, v), _) =>
              extractBindingVarRef(a).flatMap {
                case ref if isParam(ref) || !isConstant(a) => Some(Eq(Var(ref), Cast(valueToTerm(v).get, argTy(a))))
                case _ => None
              }
            } ++ ats
          case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  // Replace all terms with their constants if possible
  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    if (mayEliminate(term)) {
      transformTerm(term) match
        case None => super.visitTerm(term)
        case Some(trans) =>
          logOptimizationStat("constant term", 1, _+1)
          term match
            case Cast(_, ty) =>
              /* We need this cast here to guarantee that we don't break programs.
               * E.g. consider the following simple example:
               * R(return$2: TAny) {
               *	Q(i: >TAny< :: 4)
               *	return$2: >TAny< == i :: 4
               * }
               * The program was well-typed, but after replacing i with 4 in the Eq-Constraint, we get a type error.
               * i had type TAny, however, the constant 4 has type TInt. That is, we now compare >TAny< to <TInt>.
               * Test: OODL -> Unit Test -> Subtyping
               */
              Seq(Cast(trans, ty))
            case _ =>
              Seq(Cast(trans, term.typ.get.ty))
    } else {
      super.visitTerm(term)
    }
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



