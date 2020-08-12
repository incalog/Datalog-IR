package inca.backend.optimize

import inca.backend.ir.GP._
import inca.frontend.fun.CompileToGP.BodyMustFail

trait Optimizer {

  def optimizeModule(module: Module): Module =
    Module(module.name, module.imports, module.pats.flatMap(optimizePattern))

  def optimizePattern(pat: Pattern): Seq[Pattern] = {
    val newbodies = pat.bodies.flatMap(body =>
      try {
        optimizeBody(body)
      } catch {
        case BodyMustFail => Seq()
      }
    )
    Seq(Pattern(pat.vis, pat.name, pat.params, newbodies))
  }

  def optimizeBody(body: Body): Seq[Body] =
    Seq(Body(body.constraints.flatMap(optimizeConstraint)))

  def optimizeConstraint(con: Constraint): Seq[Constraint] = con match {
    case Call(name, args, transitive, neg) => Seq(Call(name, args.map(optimizeTerm), transitive, neg))
    case Compare(comp, lhs, rhs) => Seq(Compare(comp, optimizeTerm(lhs), optimizeTerm(rhs)))
    case HasType(t, typ) => Seq(HasType(optimizeTerm(t), typ))
    case Path(src, trg, link, targetType) => Seq(Path(optimizeTerm(src), optimizeTerm(trg), link, targetType))
    case Computed(resultVar, computation) => Seq(Computed(resultVar, computation))
  }

  def optimizeTerm(term: Term): Term = term
}
