package inca.backend.optimize
import inca.backend.ir.GP._
import inca.frontend.fun.CompileToGP.BodyMustFail
import inca.runtime.context.LanguageMetaInfo

object FoldConstantConstraints extends Optimization {

  override def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer = new Optimizer {
    override def optimizeConstraint(con: Constraint): Seq[Constraint] = con match {
      case Compare(EqComparator, t1, t2) if t1 == t2 => Seq()
      case Compare(EqComparator, Constant(c1), Constant(c2)) if c1 != c2 => throw BodyMustFail
      case Compare(NeqComparator, t1, t2) if t1 == t2 => throw BodyMustFail
      case Compare(NeqComparator, Constant(c1), Constant(c2)) if c1 == c2 => Seq()
      case _ => Seq(con)
    }
  }
}
