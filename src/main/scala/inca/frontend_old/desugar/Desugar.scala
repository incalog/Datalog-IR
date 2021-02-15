package inca.frontend_old.desugar

import inca.frontend_old.core.tree.Module
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
        module = transModule
        trans.changesMade
      }
    } while (changed)
    module
  }
}
