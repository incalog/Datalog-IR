package inca.backend.optimize

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog.{Atom, Call, Computed, CountAggregation, CustomAggregation, ExtensionalCall, Module, Name, Pattern, throwBodyMustFail}
import inca.runtime.context.DataModel

/* Eliminates non-productive relations and their calls.
 * A relation is non-productive if none of its bodies can ever produce a tuple.
 * This optimization first enumerates all productive relations and eliminates all others.
 */
object EliminateNonproductiveRelations extends Optimization {


  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    var productivePats: Set[Name] = Set()

    override def optimizeModule(module: Module): Module = {
      productivePats = Set()
      var dirty = true
      while (dirty) {
        dirty = false
        module.pats.foreach { pat =>
          if (!productivePats.contains(pat.name) && isProductive(pat)) {
            productivePats += pat.name
            dirty = true
          }
        }
      }

      // Since all pattern without a call are considered productive, we remove
      // all patterns that are not called and are not a main pattern in a second step
      /*val mainPats = module.pats.filter(_.hasHint(MagicSetHints.MainKey)).map(_.name)
      productivePats = productivePats.filter { patName =>
        lazy val isMain = mainPats.contains(patName)
        lazy val inCall = module.pats.exists { p =>
          p.bodies.exists { b =>
            b.atoms.exists {
              case Call(name, _, _, _) => name == patName
              case Computed(_, CustomAggregation(_, _, _, name, _, _)) => name == patName
              case Computed(_, CountAggregation(name, _)) => name == patName
              case _ => false
            }
          }
        }
        isMain || inCall
      }*/

      super.optimizeModule(module)
    }

    @inline
    def isProductive(pat: Pattern): Boolean =
      pat.bodies.exists(b => b.atoms.forall {
        case Call(name, _, _, _) => productivePats.contains(name)
        case _ => true
      })

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val newpats = super.optimizePattern(pat)
      newpats.filter(p => !p.isEmpty && productivePats.contains(p.name))
    }

    override def optimizeAtom(atom: Atom): Seq[Atom] = atom match {
      case Call(name, _, _, false) if !productivePats.contains(name) =>
        throwBodyMustFail()
      case _ => Seq(atom)
    }
  }
}
