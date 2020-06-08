package org.inca.trans.gp

import org.inca.lang.GraphPatternLang._

object CollectGPVars extends CollectGP[String] {
  override def transVar(v: Var): Seq[String] = Seq(v.name)
}

object CollectGPLits extends CollectGP[Literal] {
  override def transLit(lit: Literal): Seq[Literal] = Seq(lit)
}

trait CollectGP[R] {

  def apply(pat: GraphPattern): Seq[R] = {
    val paramsRes = pat.params.flatMap(transParam)
    paramsRes ++ pat.bodies.flatMap(transAlternative)
  }

  def transParam(param: Param): Seq[R] = Seq()

  def transAlternative(alt: Alternative): Seq[R] = alt.constraints.flatMap(transConstraint)

  def transConstraint(const: Constraint): Seq[R] = const match {
    case Composition(call, neg) => call.args.flatMap(transValue)
    case Compare(comp, lhs, rhs) => transValue(lhs) ++ transValue(rhs)
    case Concept(v, typ) => transValue(v)
    case Path(src, trg, link, typ) => transValue(src) ++ transValue(trg)
    case Check(code) => transCode(code)
  }

  def transValue(v: Value): Seq[R] = v match {
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
