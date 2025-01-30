package inca.ir.optimize

import inca.ir
import inca.util.{Memoize, memoize}
import inca.ir.Hint.preserveHints
import inca.ir.analysis.IRConstantAbstractInterpreter
import inca.ir.analysis.base.ordering.BaseEqOps
import inca.ir.analysis.base.values.{ConstantRelation, Value}
import inca.ir.{Arg, Atom, Body, Call, Cast, Eq, ExtensionalRelation, MainHint, ModuleEntry, Param, Ref, RefByName, Relation, Term, TermArg, Type, Var, WildcardArg}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.string as irstr
import inca.ir.extension.data as irdata
import inca.ir.extension.aggregate as iragg
import sturdy.values.Topped

extension [T](topped: Topped[T])
  def isTrue: Boolean = topped.isActual && topped.get == true
  def isFalse: Boolean = topped.isActual && topped.get == false

trait ConstantBaseIROptimizer(val interRelational: Boolean) extends BaseIROptimizer[Value, ConstantRelation, Value]:
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

  override def getBodyResult(body: Body): Set[ConstantRelation] =
    body.getAnalysisResult(BodyKey).map(_.res)

  override def getRelationResult(relation: Relation): Set[ConstantRelation] =
    relation.getAnalysisResult(RelationKey).map(_.res)

  private def relationAlwaysFails(relation: Relation): Boolean =
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
          val (paramNames, args) = params.map(p => (p.name.name, Value.Top)).unzip
          val empty = if (assumeEdbIsNotEmpty) Topped.Actual(false) else Topped.Top
          abstractInterpreter.insertEDB(n.name, ConstantRelation(paramNames, args, empty))
        case _ => // nothing
      }
    }
    super.analyzeProgram(modules)

  // Override the internal method in subclasses
  lazy val valueToTerm: Memoize[Value, Option[Term]] = memoize(valueToTermInternal)
  def valueToTermInternal(value: Value): Option[Term] =
    None

  private def transformTerm(term: Term): Option[Term] =
    val option = getTermResult(term).headOption
//    println(s"  elim $term => $option")
    option.flatMap(valueToTerm.apply) match
      case Some(value) => if (value == term) None else Some(value)
      case None => None

  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    // Remove empty relations. We know that there can not be any call site for these relations, because a failing
    // call will lead to a failing body at the call site. Except if the call is a negative call, in which case it
    // always succeeds.
    if (relationAlwaysFails(relation)) {
      logOptimizationStat("constant failed relation", 1, _+1)
      if (relationUsedInAggregation(relation))
        Seq(relation.copy(bodies = Seq()))
      else
        Seq()
    } else if (relation.getHint(MainHint).isEmpty) {
      val res = getRelationResult(relation).headOption.get
      val nonconstantParams = relation.params.zip(res.rows).flatMap {
        case (p, v) if v.isConstant && mayEliminate(p)(relation) =>
          None
        case (p, _) => Some(p)
      }
      if (nonconstantParams.isEmpty) {
        logOptimizationStat("constant relation", 1, _+1)
        if (relationUsedInAggregation(relation))
          Seq(relation.copy(bodies = Seq()))
        else
          Seq()
      } else {
        val k = relation.params.size - nonconstantParams.size
        if (k != 0)
          logOptimizationStat("constant relation parameter", k, _ + k)
        super.visitRelation(relation.copy(params = nonconstantParams))
      }
    } else {
      val r = super.visitRelation(relation)
      r
    }
  }

  override def visitBody(body: Body): Seq[Body] = preserveHints(body) {
//    // When we don't differentiate call sites, we might end up with two different annotations (e.g. SomeConst and Top)
//    // for the same variable. We can not remove bindings equality constraints for these.
//    relevantBodyVars = body.vars.filter { t =>
//      t.mode.isBound && transformTerm(t).isEmpty
//    }.map(_.ref).toSet

    if (bodyAlwaysFails(body)) {
      logOptimizationStat("constant failed body", 1, _+1)
      Seq()
    } else {
      super.visitBody(body).filter(_.atoms.nonEmpty)
    }
  }

  protected def binCompare[A](lhs: Term, rhs: Term, op: (Value, Value) => A): Option[A] =
    (getTermResult(lhs).headOption, getTermResult(rhs).headOption) match
      case (Some(v1), Some(v2)) => Some(op(v1, v2))
      case _ => None

  protected def mayEliminate(p: Param)(implicit relation: Relation): Boolean =
    true

  protected def mayEliminate(t: Term): Boolean =
    //val b = !t.typ.get.mode.isBinding || t.isInstanceOf[Var] && !params.contains(t.asInstanceOf[Var].ref)
    //if (b) println(s"may elim $t") else println(s"may NOT elim $t")
    //b
    t match
      case _: Var => !params.contains(t.asInstanceOf[Var].ref)
      case _ => true

  protected def extractBindingVarRef(arg: Arg): Option[Ref[Var.Target]] = arg match
    case TermArg(v@Var(ref)) if v.typ.get.mode.isBinding => Some(v.ref)
    case _ => None

  protected def argTy(arg: Arg): Type = arg match
    case TermArg(t) => t.typ.get.ty
    case w@WildcardArg() => w.typ.get.ty

  protected def mayEliminate(eq: Eq): Boolean = 
    eq.neg || (mayEliminate(eq.lhs) && mayEliminate(eq.rhs))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case eq@Eq(lhs, rhs, neg) if mayEliminate(eq) =>
        val op = if (neg) eqOps.neq else eqOps.equ
        val comp = binCompare(lhs, rhs, op(_, _))
        if (comp.exists(_.isTrue) || eq.isAssignment && getTermResult(eq.lhs).headOption.exists(_.isConstant)) {
//          println(s"Eq $eq, yes")
          logOptimizationStat("constant equation", 1, _ + 1)
          Seq()
        } else {
//          println(s"Eq $eq, no")
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
        /*val argsAllBounds = args.forall {
          case TermArg(t) => t.typ.get.mode.isBound
          case AggregateColumnArg(t) => t.typ.get.mode.isBound
          case WildcardArg() => true
          case _ => false
        }*/
        ref.target match
          /*case Some(r: Relation) if relationAlwaysSucceeds(r) && argsAllBounds =>
            // Remove containment checks if they always succeed (run wildcard detection first)
            logOptimizationStat("bound failed call", 1, _+1)
            Seq()*/
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

            // In case we have removed an argument that was binding a parameter, we need to insert an equality
            // constraint for that parameter.
            // Test (general problem): Datalog frontend -> lecture 5 -> nat relation
            // Test (why cast is needed): OODL -> Unit Test -> Subtyping
            constantArgs.flatMap { case ((a, v), _) =>
              extractBindingVarRef(a).map(ref => Eq(Var(ref), Cast(valueToTerm(v).get, argTy(a))))
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
            case _ => Seq(Cast(trans, term.typ.get.ty))
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



