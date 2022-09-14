package inca.backend.optimize

import inca.backend.ir.Datalog.{Atom, Call, Module, Name, Pattern, throwBodyMustFail}
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

      super.optimizeModule(module)
    }

    @inline
    def isProductive(pat: Pattern): Boolean =
      pat.bodies.exists(b => b.atoms.forall {
        case Call(name, _, _, _) =>
        println(pat.name, name, productivePats.contains(name), productivePats)
          productivePats.contains(name)
        case _ => true
      })

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val newpats = super.optimizePattern(pat)
      newpats.filter(!_.isEmpty)
    }

    override def optimizeAtom(atom: Atom): Seq[Atom] = atom match {
      case Call(name, _, _, false) if !productivePats.contains(name) =>
        println("Check: ", name, productivePats)
        throwBodyMustFail()
      case _ => Seq(atom)
    }
  }
}
