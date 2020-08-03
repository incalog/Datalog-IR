package inca.lang.funext.desugar

import inca.lang.fun.Fun.Module
import inca.util.Gensym

object Desugar {

  def apply(_desugarables: Desugarable*)(_module: Module): Module = {
    val desugarables: Seq[Desugarable] = {
      var allDesugarables = _desugarables.distinct
      var newDesugarables = Seq[Desugarable]()
      do {
        allDesugarables = allDesugarables ++ newDesugarables
        newDesugarables = allDesugarables.flatMap(_.desugarsTo) diff allDesugarables
      } while (newDesugarables.nonEmpty)

      allDesugarables
    }

    implicit val gensym: Gensym = new Gensym(Iterable())
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
