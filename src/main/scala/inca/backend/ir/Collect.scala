package inca.backend.ir

import inca.backend.ir.Datalog._

object CollectVars extends Collect[String] {
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}

object CollectLits extends Collect[Literal] {
  override def transLit(lit: Literal): Seq[Literal] = Seq(lit)
}

object CollectConstantEvaluation extends Collect[Evaluation] {
  override def transComputation(computation: Computation): Seq[Evaluation] = computation match {
    case eval: Evaluation if eval.args.isEmpty => Seq(eval)
    case _ => super.transComputation(computation)
  }
}

trait Collect[R] {

  def apply(mod: Module): Seq[R] = {
    mod.pats.flatMap(transPattern)
  }

  def transPattern(pat: Pattern): Seq[R] = {
    val paramsRes = pat.params.flatMap(transParam)
    paramsRes ++ pat.bodies.flatMap(transBody)
  }

  def transParam(param: Param): Seq[R] = Seq()

  def transBody(alt: Body): Seq[R] = alt.atoms.flatMap(transAtom)

  def transAtom(atom: Atom): Seq[R] = atom match {
    case Call(_, args, _, _) => args.flatMap(transTerm)
    case ExtensionalCall(_, args, _) => args.flatMap(transTerm)
    case Compare(_, lhs, rhs) => transTerm(lhs) ++ transTerm(rhs)
    case HasType(v, _) => transTerm(v)
    case NotHasType(v, _) => transTerm(v)
    case Path(src, _, _, trg, _) => transTerm(src) ++ transTerm(trg)
    case NoPath(t, _, _, _) => transTerm(t)
    case Computed(lhs, comp) => transTerm(lhs) ++ transComputation(comp)
    case Undef(t) => transTerm(t)
  }

  def transTerm(v: Term): Seq[R] = v match {
    case vari@Var(_) => transVar(vari)
    case Constant(lit) => transLit(lit)
  }

  def transVar(v: Var): Seq[R] = Seq()

  def transLit(lit: Literal): Seq[R] = lit match {
    case IntLiteral(_) => Seq()
    case LongLiteral(_) => Seq()
    case DoubleLiteral(_) => Seq()
    case StringLiteral(_) => Seq()
    case BooleanLiteral(_) => Seq()
  }

  def transComputation(computation: Computation): Seq[R] = computation match {
    case CountAggregation(_, args) => args.flatMap(transTerm)
    case Evaluation(args, _, _) => args.flatMap(v => transTerm(v._1))
    case CustomAggregation(_, _, _, _, args, _) => args.flatMap(transTerm)
  }
}
