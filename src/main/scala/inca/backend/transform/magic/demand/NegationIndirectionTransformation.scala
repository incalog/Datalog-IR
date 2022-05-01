package inca.backend.transform.magic.demand

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Body, Call, Module, Pattern, Var}
import inca.backend.transform.{Transformation, Transformer}
import inca.runtime.context.DataModel
import inca.backend.hints.MagicSetHints.NegativeIndirectionRelation


object NegationIndirectionTransformation extends Transformation {

  def negativeIndirectionName(name: String): String = "negative$" + name

  override def transformer(dataModel: DataModel): Transformer = new Transformer {
    var negativelyCalledPatterns: Set[Call] = Set()

    override def transformModule(module: Module): Module = {
      val transformedPats = module.pats.flatMap(transformPattern)
      val patterns = module.pats.map(p => p.name -> p).toMap
      val negIndirectPatterns = negativelyCalledPatterns.map { call =>
        val pat = patterns(call.name)
        val args = pat.params.map(p => Var(p.name))
        Pattern(pat.vis, negativeIndirectionName(call.name), pat.params, Seq(Body(Seq(
          Call(call.name, args, call.transitive, neg = true)
        )))).addHint(NegativeIndirectionRelation)
      }
      val newpats = transformedPats ++ negIndirectPatterns
      Module(module.name, module.imports, newpats, module.scalaContent)
    }

    override def transformAtom(atom: Datalog.Atom): Seq[Datalog.Atom] = (atom match {
      case x@Call(name, args, transitive, true) =>
        negativelyCalledPatterns += x
        Seq(Call(negativeIndirectionName(name), args.map(transformTerm), transitive, neg = false))
      case _ => super.transformAtom(atom)
    }).map(_.withHints(atom))
  }
}
