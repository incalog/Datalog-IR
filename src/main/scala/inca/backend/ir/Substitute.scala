package inca.backend.ir

import inca.backend.ir.GP._

case class Substitute(subst: Var => Term) {

  def substModule(module: Module): Module =
    Module(module.name, module.imports, module.pats.map(substPattern))

  def substPattern(pat: Pattern): Pattern =
    Pattern(pat.vis, pat.name, pat.params, pat.bodies.map(substBody))

  def substBody(body: Body): Body =
    Body(body.constraints.map(substConstraint))

  def substConstraint(con: Constraint): Constraint = con match {
    case Call(name, args, transitive, neg) => Call(name, args.map(substTerm), transitive, neg)
    case Compare(comp, lhs, rhs) => Compare(comp, substTerm(lhs), substTerm(rhs))
    case HasType(t, typ) => HasType(substTerm(t), typ)
    case NotHasType(t, typ) => NotHasType(substTerm(t), typ)
    case Path(src, srcTy, link, trg, trgTy) => Path(substTerm(src), srcTy, link, substTerm(trg), trgTy)
    case NoPath(t, ty, link, termIsSource) => NoPath(substTerm(t), ty, link, termIsSource)
    case Computed(lhs, computation) => Computed(substTerm(lhs), substComputation(computation))
  }

  def substTerm(term: Term): Term = term match {
    case v: Var => subst(v)
    case c: Constant => c
  }

  def substComputation(comp: Computation): Computation = comp match {
    case CountAggregation(patName, args) => CountAggregation(patName, args.map(substTerm))
    case Evaluation(args, resultType, code) => Evaluation(args.map(a => substTerm(a._1) -> a._2), resultType, code)
    case LatticeAggregation() => ???
  }
}

object Substitute {
  def fromMap(m: Map[Var, Term]) = new Substitute(v => m.getOrElse(v, v))
}
