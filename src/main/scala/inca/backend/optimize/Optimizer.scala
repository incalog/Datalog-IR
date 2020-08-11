package inca.backend.optimize

import inca.backend.ir.GP._
import inca.frontend.fun.CompileToGP.BodyMustFail

trait Optimizer {

  def optimizeModule(module: Module): Module =
    Module(module.name, module.imports, module.pats.flatMap(optimizePattern))

  def optimizePattern(pat: Pattern): Seq[Pattern] =
    Seq(Pattern(pat.vis, pat.name, pat.params, pat.bodies.flatMap(optimizeBody)))

  def optimizeBody(body: Body): Seq[Body] =
    try {
      Seq(Body(body.constraints.flatMap(optimizeConstraint)))
    } catch {
      case BodyMustFail => Seq()
    }

  def optimizeConstraint(con: Constraint): Seq[Constraint] = con match {
    case Call(name, args, transitive, neg) => Seq(Call(name, args.map(optimizeTerm), transitive, neg))
    case Compare(comp, lhs, rhs) => Seq(Compare(comp, optimizeTerm(lhs), optimizeTerm(rhs)))
    case HasType(t, typ) => Seq(HasType(optimizeTerm(t), typ))
    case Path(src, trg, link, targetType) => Seq(Path(optimizeTerm(src), optimizeTerm(trg), link, targetType))
    case Computed(resultVar, computation) => Seq(Computed(resultVar, computation))
  }

  def optimizeTerm(term: Term): Term = term
}
