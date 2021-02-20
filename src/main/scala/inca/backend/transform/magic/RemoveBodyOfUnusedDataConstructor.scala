package inca.backend.transform.magic

import inca.backend.hints.DataHints
import inca.backend.ir.Collect
import inca.backend.ir.GP._
import inca.backend.transform.{Transformation, Transformer}

object RemoveBodyOfUnusedDataConstructor extends Transformation  {
  override def transformer: Transformer = new Transformer {
    override def transformModule(mod: Module): Module = {
      val (cotrPats, nonCotrPats) = mod.pats.partition(_.hasHint(DataHints.ConstructorKey))
      val nonDataPats = mod.pats.filter(pat => !pat.hasHint(DataHints.SelectorKey) && !pat.hasHint(DataHints.DataTypeKey))
      val calledPatsInNonData = nonDataPats.flatMap(CollectCalledPatterns.transPattern)
      val modifiedCotrPats = cotrPats.map { pat =>
        // figure out if cotr is called in module
        val callsPat = calledPatsInNonData.contains(pat.name)
        val bodies =
          if (!callsPat) pat.bodies.filter(b => !b.hasHint(DataHints.IDBConstructorKey))
          else pat.bodies
        Pattern(pat.vis, pat.name, pat.params, bodies).withHints(pat)
      }

      Module(mod.name, mod.imports, nonCotrPats ++ modifiedCotrPats, mod.scalaContent)
    }
  }

  object CollectCalledPatterns extends Collect[String] {
    override def transConstraint(const: Constraint): Seq[Name] = const match {
      case Call(name, _, _, _) => Seq(name)
      case _ => Seq()
    }
  }

}
