package inca.backend.ir

import inca.backend.ir.GP._

object CollectVars extends Collect[String] {
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}

object CollectComputedConstantEvals extends Collect[(Term, ConstantEvaluation)] {
  override def transConstraint(const: Constraint): Seq[(Term, ConstantEvaluation)] = const match {
    case Computed(lhs, e@ConstantEvaluation(_, _)) => Seq((lhs, e))
    case _ => Seq()
  }
}

object CollectConstantEvaluationLhs extends Collect[String] {
  override def transConstraint(const: Constraint): Seq[String] = const match {
    case Computed(Var(name), ConstantEvaluation(_, _)) => Seq(name)
    case _ => Seq()
  }
}

object CollectLits extends Collect[Literal] {
  override def transLit(lit: Literal): Seq[Literal] = Seq(lit)
}

trait Collect[R] {

  def apply(pat: Pattern): Seq[R] = {
    val paramsRes = pat.params.flatMap(transParam)
    paramsRes ++ pat.bodies.flatMap(transBody)
  }

  def transParam(param: Param): Seq[R] = Seq()

  def transBody(alt: Body): Seq[R] = alt.constraints.flatMap(transConstraint)

  def transConstraint(const: Constraint): Seq[R] = const match {
    case Call(_, args, _, _) => args.flatMap(transTerm)
    case Compare(comp, lhs, rhs) => transTerm(lhs) ++ transTerm(rhs)
    case HasType(v, typ) => transTerm(v)
    case NotHasType(v, typ) => transTerm(v)
    case Path(src, srcTy, link, trg, trgTy) => transTerm(src) ++ transTerm(trg)
    case NoPath(t, ty, link, termIsSource) => transTerm(t)
    case Computed(lhs, comp) => transTerm(lhs) ++ transComputation(comp)
  }

  def transTerm(v: Term): Seq[R] = v match {
    case vari@Var(name) => transVar(vari)
    case Constant(lit) => transLit(lit)
  }

  def transVar(v: Var): Seq[R] = Seq()

  def transLit(lit: Literal): Seq[R] = lit match {
    case IntLiteral(v) => Seq()
    case LongLiteral(v) => Seq()
    case DoubleLiteral(v) => Seq()
    case StringLiteral(v) => Seq()
    case BooleanLiteral(v) => Seq()
  }

  def transComputation(computation: Computation): Seq[R] = computation match {
    case CountAggregation(_, args) => args.flatMap(transTerm)
    case ConstantEvaluation(_, _) => Seq()
    case Evaluation(args, _, _) => args.flatMap(v => transTerm(v._1)).toSeq
    case CustomAggregation(_, _, _, _, _, args, _) => args.flatMap(transTerm)
  }
}
