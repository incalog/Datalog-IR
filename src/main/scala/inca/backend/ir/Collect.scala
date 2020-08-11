package inca.backend.ir

import inca.backend.ir.GP._

object CollectVars extends Collect[String] {
  override def transVar(v: Var): Seq[String] = Seq(v.name)
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
    case Path(src, trg, link, ty) => transTerm(src) ++ transTerm(trg)
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
    case CountAggregation(patName, args) => args.flatMap(transTerm)
    case Evaluation(freeVars, _, _) => freeVars.flatMap(transTerm).toSeq
    case LatticeAggregation() => ???
  }
}
