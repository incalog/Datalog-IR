package inca.backend.optimize
import inca.backend.ir.Datalog.{Body, Call, Module, Pattern, Var}
import inca.runtime.context.DataModel
import inca.util.{Gensym, Scala}

object ExtractLargeBodies extends Optimization {
  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {
    val gensym = new Gensym(Seq.empty)

    override def optimizeModule(module: Module): Module = {
      val patNames = module.pats.map(_.name)
      gensym.register(it = patNames)

      super.optimizeModule(module)
    }

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val Pattern(vis, name, params, bodies) = pat

      val maxNumAtoms = 600
      var bodiesBySize = bodies.sortBy(_.atoms.size)
      var numAtoms = bodies.map(_.atoms.size).sum

      var extractedBodies: Seq[Body] = Seq()
      while (numAtoms > maxNumAtoms) {
        val (head :: tail) = bodiesBySize
        numAtoms -= head.atoms.size
        bodiesBySize = tail
        extractedBodies :+= head
      }

      if (extractedBodies.nonEmpty) {
        val extractedPatternName = gensym.fresh(name)
        val callExtractedPatternBody = Body(Seq(
          Call(extractedPatternName, params.map(p => Var(p.name)))
        ))

        val newPat = Pattern(vis, extractedPatternName, params, extractedBodies)
        optimizePattern(newPat) :+ Pattern(vis, name, params, bodiesBySize :+ callExtractedPatternBody)
      } else {
        Seq(pat)
      }
    }
  }
}
