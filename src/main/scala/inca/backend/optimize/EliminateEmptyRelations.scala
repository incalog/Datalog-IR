package inca.backend.optimize

import inca.backend.ir.GP.{Call, Constraint, Module, Name, Pattern, throwBodyMustFail}
import inca.runtime.context.LanguageMetaInfo

object EliminateEmptyRelations extends Optimization {


  override def optimizer(languageMetaInfo: LanguageMetaInfo): Optimizer = new Optimizer {

    var pats: Map[Name, Pattern] = Map()
    var dirty = true

    override def optimizeModule(module: Module): Module = {
      pats = module.pats.map(p => p.name -> p).toMap
      var m = module
      while (dirty) {
        dirty = false
        m = super.optimizeModule(m)
      }
      m
    }

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val newpats = super.optimizePattern(pat)
      pats ++= newpats.map(p => p.name -> p)
      newpats.filter(!_.isEmpty)
    }

    override def optimizeConstraint(con: Constraint): Seq[Constraint] = con match {
      case Call(name, _, _, false) if pats(name).isEmpty =>
        dirty = true
        throwBodyMustFail()
      case _ => Seq(con)
    }
  }
}
