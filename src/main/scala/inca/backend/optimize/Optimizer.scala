package inca.backend.optimize

import inca.backend.ir.GP._
import inca.frontend_old.core.CompileToGP.BodyMustFail

trait Optimizer {

  def optimizeModule(module: Module): Module =
    Module(module.name, module.imports, module.data, module.pats.flatMap(optimizePattern), module.scalaContent)

  def optimizePattern(pat: Pattern): Seq[Pattern] = {
    val newbodies = pat.bodies.flatMap(body =>
      try {
        optimizeBody(body, pat)
      } catch {
        case BodyMustFail => Seq()
      }
    )
    Seq(Pattern(pat.vis, pat.name, pat.params, newbodies).withHints(pat))
  }

  def optimizeBody(body: Body, pat: Pattern): Seq[Body] =
    Seq(Body(body.constraints.flatMap(optimizeConstraint)).withHints(body))

  def optimizeConstraint(con: Constraint): Seq[Constraint] = (con match {
    case Call(name, args, transitive, neg) => Seq(Call(name, args.map(optimizeTerm), transitive, neg))
    case ExtensionalCall(name, args, neg) => Seq(ExtensionalCall(name, args, neg))
    case Compare(comp, lhs, rhs) => Seq(Compare(comp, optimizeTerm(lhs), optimizeTerm(rhs)))
    case HasType(t, typ) => Seq(HasType(optimizeTerm(t), typ))
    case NotHasType(t, typ) => Seq(NotHasType(optimizeTerm(t), typ))
    case Path(src, srcTy, link, trg, trgTy) => Seq(Path(optimizeTerm(src), srcTy, link, optimizeTerm(trg), trgTy))
    case NoPath(t, ty, link, termIsSource) => Seq(NoPath(optimizeTerm(t), ty, link, termIsSource))
    case Computed(resultVar, computation) => Seq(Computed(resultVar, computation))
    case Undef(t) => Seq(Undef(optimizeTerm(t)))
  }).map(_.withHints(con))

  def optimizeTerm(term: Term): Term = term
}
