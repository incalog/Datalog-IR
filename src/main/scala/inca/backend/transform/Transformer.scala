package inca.backend.transform

import inca.backend.ir.IR._

trait Transformer {

  def transformModule(module: Module): Module =
    Module(module.name, module.imports, module.pats.flatMap(transformPattern), module.scalaContent)

  def transformPattern(pat: Pattern): Seq[Pattern] = {
    val newbodies = pat.bodies.flatMap(body =>
      try {
        transformBody(body, pat)
      } catch {
        case BodyMustFail => Seq()
      }
    )
    Seq(Pattern(pat.vis, pat.name, pat.params, newbodies).withHints(pat))
  }

  def transformBody(body: Body, pat: Pattern): Seq[Body] =
    Seq(Body(body.atoms.flatMap(transformAtom)).withHints(body))

  def transformAtom(atom: Atom): Seq[Atom] = (atom match {
    case Call(name, args, transitive, neg) => Seq(Call(name, args.map(transformTerm), transitive, neg))
    case ExtensionalCall(name, args, neg) => Seq(ExtensionalCall(name, args, neg))
    case Compare(comp, lhs, rhs) => Seq(Compare(comp, transformTerm(lhs), transformTerm(rhs)))
    case HasType(t, typ) => Seq(HasType(transformTerm(t), typ))
    case NotHasType(t, typ) => Seq(NotHasType(transformTerm(t), typ))
    case Path(src, srcTy, link, trg, trgTy) => Seq(Path(transformTerm(src), srcTy, link, transformTerm(trg), trgTy))
    case NoPath(t, ty, link, termIsSource) => Seq(NoPath(transformTerm(t), ty, link, termIsSource))
    case Computed(resultVar, computation) => Seq(Computed(resultVar, computation))
    case Undef(t) => Seq(Undef(transformTerm(t)))
  }).map(_.withHints(atom))

  def transformTerm(term: Term): Term = term
}
