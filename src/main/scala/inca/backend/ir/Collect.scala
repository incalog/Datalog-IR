package inca.backend.ir

trait CollectVarNames[D <: DatalogGeneric] extends Collect[D] {
  import datalog._
  override type R = String
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}
trait CollectVars[D <: DatalogGeneric] extends Collect[D] {
  import datalog._
  override type R = Var
  override def transVar(v: Var): Seq[Var] = Seq(v)
}
trait CollectLits[D <: DatalogGeneric] extends Collect[D] {
  import datalog._
  override type R = base.Literal
  override def transLit(lit: base.Literal): Seq[base.Literal] = Seq(lit)
}

trait CollectConstantEvaluation[D <: DatalogGeneric] extends Collect[D] {
  import datalog._
  override type R = Evaluation
  override def transComputation(computation: Computation): Seq[Evaluation] = computation match {
    case eval: Evaluation if eval.args.isEmpty => Seq(eval)
    case _ => super.transComputation(computation)
  }
}

trait Collect[D <: DatalogGeneric] {
  type R

  val datalog: D
  import datalog._

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
    case vari @ Var(_) => transVar(vari)
    case Constant(lit) => transLit(lit)
  }

  def transVar(v: Var): Seq[R] = Seq()

  def transLit(lit: base.Literal): Seq[R] = lit match {
    case base.IntLiteral(_) => Seq()
    case base.LongLiteral(_) => Seq()
    case base.DoubleLiteral(_) => Seq()
    case base.StringLiteral(_) => Seq()
    case base.BooleanLiteral(_) => Seq()
  }

  def transComputation(computation: Computation): Seq[R] = computation match {
    case CountAggregation(_, args) => args.flatMap(transTerm)
    case Evaluation(args, _, _) => args.flatMap(v => transTerm(v._1))
    case CustomAggregation(_, _, _, _, args, _) => args.flatMap(transTerm)
  }
}
