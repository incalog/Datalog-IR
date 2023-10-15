package inca.backend.optimize
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.Atom
import inca.runtime.context.DataModel
import org.eclipse.collections.api.RichIterable
import org.eclipse.collections.api.multimap.MutableMultimap
import org.eclipse.collections.api.tuple.Pair
import org.eclipse.collections.impl.factory.{Multimaps, Sets}

object EliminateClones extends Optimization {
  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {

    val atoms: MutableMultimap[Atom, (String, Int)] = Multimaps.mutable.set.empty()

    override def optimizeModule(module: Datalog.Module): Datalog.Module = {
      atoms.clear()
      super.optimizeModule(module)
      var count = 0
      val minSize = 3

      val clonedCalls = atoms.keyMultiValuePairsView().toList
        .select(p => p.getOne.isInstanceOf[Datalog.Call] && p.getTwo.size() > 1)
        .sortThisBy(p => Integer.valueOf(- p.getTwo.size()))

      import scala.jdk.CollectionConverters._
      //clonedCalls.asScala.foreach(p => println(p.getTwo.size()+":"+p))

      atoms.forEachKey { (atom) =>
        val set = atoms.get(atom)
        if (set.size() > minSize) {
//          println(s"${set.size()}:\t$atom")
          count += 1
        }
      }


//      val positions = atoms.flip()
//      while (!atoms.isEmpty) {
//        val p = atoms.keyValuePairsView().getAny
//        val atom = p.getOne
//        val (rel, ix) = p.getTwo
//        atoms.remove(atom, (rel,ix))
//        positions.remove((rel,ix), atom)
//
//      }



      //println(s"$count atoms occur $minSize times or more")
      module
    }

    override def optimizeBody(body: Datalog.Body, pat: Datalog.Pattern): Seq[Datalog.Body] = {
      for ((atom, ix) <- body.atoms.zipWithIndex)
        atoms.put(atom, (pat.name, ix))

      Seq(body)
    }
  }
}
