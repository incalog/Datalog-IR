package inca.backend.optimize
import inca.backend.ir.GP._
import inca.backend.ir.TypeOps
import inca.frontend.core.CompileToGP.BodyMustFail
import inca.frontend.typechecker.ScalaTypeContext
import inca.runtime.context.LanguageMetaInfo

object FoldConstantConstraints extends Optimization with TypeOps {


  override def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer = new Optimizer {

    override def optimizeModule(module: Module): Module = {
      initializeScala(module)
      super.optimizeModule(module)
    }

    override def optimizeConstraint(con: Constraint): Seq[Constraint] = con match {

      case Compare(EqComparator, t1, t2) if t1 == t2 => Seq()
      case Compare(EqComparator, Constant(c1), Constant(c2)) if c1 != c2 => throw BodyMustFail

      case Compare(NeqComparator, t1, t2) if t1 == t2 => throw BodyMustFail
      case Compare(NeqComparator, Constant(c1), Constant(c2)) if c1 == c2 => Seq()

      case HasType(t, typ) =>
        val termTyp = t match {
          case v:Var => v.typ.getOrElse(TAny)
          case c:Constant => c.lit.typ
        }
        if (termTyp == typ) {
          // this constraint was responsible for the inferrence of termTyp, must keep it
          Seq(con)
        } else {
          val meetType = meet(termTyp, typ, languageMetaInfo)
          if (meetType.contains(termTyp)) {
            // upcast, always succeeds
            Seq()
          } else if (meetType.contains(typ)) {
            // downcast, makes sense
            Seq(con)
          } else if (meetType.isEmpty) {
            // cast to unrelated type, cannot succeed
            throw BodyMustFail
          } else {
            throw new IllegalArgumentException
          }
        }

      case NotHasType(t, typ) =>
        val termTyp = t match {
          case v:Var => v.typ.getOrElse(TAny)
          case c:Constant => c.lit.typ
        }
        val meetType = meet(termTyp, typ, languageMetaInfo)
        if (meetType.contains(termTyp)) {
          // termTyp <: typ, hence NotHasType must fail
          throw BodyMustFail
        } else if (meetType.contains(typ)) {
          // termTyp :> typ, hence NotHasType makes sense
          Seq(con)
        } else if (meetType.isEmpty) {
          // termTyp and typ are unrelated, NotHasType always succeeds
          Seq()
        } else {
          throw new IllegalArgumentException
        }

      case _ => Seq(con)
    }
  }
}
