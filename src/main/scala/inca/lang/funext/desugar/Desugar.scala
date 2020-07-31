package inca.lang.funext.desugar

import inca.lang.fun.Fun.Module
import inca.util.Gensym

object Desugar {

  def apply(_desugarables: Desugarable*)(_module: Module): Module =
    apply(_desugarables.toSet)(_module)

  def apply(_desugarables: Set[Desugarable])(_module: Module): Module = {
    val desugarables: Vector[Desugarable] = {
      var allDesugarables = _desugarables
      var newDesugarables = Set[Desugarable]()
      do {
        allDesugarables = allDesugarables ++ newDesugarables
        newDesugarables = allDesugarables.flatMap(_.desugarsTo) -- allDesugarables
      } while (newDesugarables.nonEmpty)

      allDesugarables.toVector
    }

    implicit val gensym: Gensym = new Gensym(_module.usedvars)
    var changed = false
    var module = _module
    do {
      changed = desugarables.exists { desugarable =>
        val trans = desugarable.trans()
        val transModule = trans.desugarModule(module)
        if (trans.changesMade) {
          module = transModule
          true
        } else {
          false
        }
      }
    } while (changed)
    module
  }
}
