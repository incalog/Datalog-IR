package inca.backend.optimize
import inca.backend.ir.Datalog.{Body, Call, Module, Pattern, Var}
import inca.runtime.context.DataModel
import inca.util.Gensym

// If a pattern has too many atoms, the generated Java Method for the pattern will exceed 65535 bytes. To prevent this
// issue, we limit pattern to a total of 600 atoms. If a pattern contains more than 600 atoms, bodies will be outlined
// to new relations, until the pattern contains less than 600 atoms.
// Note: You should execute this optimization after all other optimization are applied. This is a last resort to
//       workaround a limitation in the JVM.
// Note: This optimization does not consider pattern with a single body that has more than 600 atoms.
object ExtractLargeBodies extends Optimization {
  // The maximum number of atoms a pattern can contain
  val maxNumAtoms = 600

  override def optimizer(dataModel: DataModel): Optimizer = new Optimizer {
    val gensym = new Gensym(Seq.empty)

    override def optimizeModule(module: Module): Module = {
      val patNames = module.pats.map(_.name)
      gensym.register(patNames)
      super.optimizeModule(module)
    }

    override def optimizePattern(pat: Pattern): Seq[Pattern] = {
      val Pattern(vis, name, params, bodies) = pat

      // Partition bodies by threshold
      val remainingBodies :: groupedBodies = bodies.sortBy(_.atoms.size).foldLeft(Seq(Seq.empty[Body])) {
        case (acc, body) =>
          val currentBin = acc.last
          val currentBinSize = currentBin.map(_.atoms.size).sum
          val bodySize = body.atoms.size

          if (currentBinSize + bodySize <= maxNumAtoms)
            acc.init :+ (currentBin :+ body)
          else
            acc :+ Seq(body)
      }

      val newPattern = groupedBodies.map(bs => Pattern(vis, gensym.fresh(name), params, bs))
      val paramArgs = params.map(p => Var(p.name))
      val newBodies = newPattern.map(p => Body(Seq(Call(p.name, paramArgs))))
      newPattern :+ Pattern(vis, name, params, remainingBodies ++ newBodies)
    }
  }
}
