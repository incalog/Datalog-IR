package inca.lang.gp

import inca.lang.gp.GP._

object CollectVars extends Collect[String] {
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}

object CollectLits extends Collect[Literal] {
  override def transLit(lit: Literal): Seq[Literal] = Seq(lit)
}

trait Collect[R] {

  def apply(pat: Rule): Seq[R] = {
    val paramsRes = pat.params.flatMap(transParam)
    paramsRes ++ pat.bodies.flatMap(transAlternative)
  }

  def transParam(param: Param): Seq[R] = Seq()

  def transAlternative(alt: Body): Seq[R] = alt.constraints.flatMap(transConstraint)

  def transConstraint(const: Atom): Seq[R] = const match {
    case Call(_, args, _, _) => args.flatMap(transValue)
    case Compare(comp, lhs, rhs) => transValue(lhs) ++ transValue(rhs)
    case HasType(v, typ) => transValue(v)
    case Path(src, trg, link) => transValue(src) ++ transValue(trg)
    case Native(code) => transCode(code)
  }

  def transValue(v: Term): Seq[R] = v match {
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

  // TODO reflection to get vars?
  def transCode(code: String): Seq[R] = Seq()
}
