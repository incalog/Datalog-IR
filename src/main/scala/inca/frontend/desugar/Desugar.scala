package inca.frontend.desugar

import inca.frontend.core.Core.Module
import inca.util.Gensym

object Desugar {

  def apply(_desugarables: Seq[Desugarable])(_module: Module): Module = {
    val desugarables: Seq[Desugarable] = {
      var allDesugarables = _desugarables.distinct
      var newDesugarables = Seq[Desugarable]()
      do {
        allDesugarables = allDesugarables ++ newDesugarables
        newDesugarables = allDesugarables.flatMap(_.desugarsTo) diff allDesugarables
      } while (newDesugarables.nonEmpty)

      allDesugarables
    }

    implicit val gensym: Gensym = new Gensym(Iterable.empty)
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
