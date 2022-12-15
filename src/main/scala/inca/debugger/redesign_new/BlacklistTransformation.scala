package inca.debugger.redesign_new

import inca.backend.analyze.DependencyGraph
import inca.backend.ir.Datalog
import inca.backend.transform.Transformation
import inca.backend.transform.Transformer
import inca.runtime.context.DataModel
import inca.util.TupleOps
import scala.collection.mutable

object BlacklistTransformation extends Transformation {

  type Adornment = Seq[Boolean]
  override def transformer(dataModel: DataModel): Transformer = new Transformer {
    var dependencyGraph: DependencyGraph = _

    private def isCyclic(p: String): Boolean =
      dependencyGraph.cycles.exists(_.contains(p))

    val idbBlacklists: mutable.Map[String, Seq[(Adornment, Datalog.Pattern)]] = mutable.Map()

    override def transformModule(module: Datalog.Module): Datalog.Module = {
      dependencyGraph = new DependencyGraph(module)
      idbBlacklists.clear()
      super.transformModule(module)
    }

    override def transformPattern(pat: Datalog.Pattern): Seq[Datalog.Pattern] = {
      // generate patterns
      if (isCyclic(pat.name)) {
        val possibleAdornments: Seq[Adornment] =
          TupleOps.cartesianProduct(pat.params.map(_ => Seq(true, false)))
        val nonboundAdornment = pat.params.map(_ => false)
        val oneBoundAdornments = possibleAdornments.diff(Seq(nonboundAdornment))

        val blacklistPatterns = oneBoundAdornments.map { adornment =>
          val name = BlacklistTransformation.blacklistName(pat.name, adornment)
          val extName = BlacklistTransformation.extBlacklistName(pat.name, adornment)
          val boundParams = pat.params.zipWithIndex.filter { case (_, idx) =>
            adornment(idx)
          }.map(_._1)
          val body = Datalog.Body(
            Seq(Datalog.ExtensionalCall(extName, boundParams.map(p => Datalog.Var(p.name)))))
          val blacklistPattern = Datalog.Pattern(None, name, boundParams, Seq(body))
          adornment -> blacklistPattern
        }

        idbBlacklists += pat.name -> blacklistPatterns
        super.transformPattern(pat) ++ blacklistPatterns.map(_._2)
      } else super.transformPattern(pat)
    }

    override def transformBody(body: Datalog.Body, pat: Datalog.Pattern): Seq[Datalog.Body] = {
      // introduce negated calls of blacklists
      if (isCyclic(pat.name)) {
        val negatedCalls = idbBlacklists(pat.name).map { case (adornment, pattern) =>
          val args = pat.params.zipWithIndex.flatMap { case (param, idx) =>
            if (adornment(idx)) Some(Datalog.Var(param.name))
            else None
          }
          Datalog.Call(pattern.name, args, neg = true)
        }
        Seq(Datalog.Body(body.atoms ++ negatedCalls))
      } else {
        Seq(body)
      }
    }
  }

  def blacklistName(p: String, adornment: Adornment): String = {
    val adornmentString = adornment.map(p => if (p) "b" else "f").mkString("")
    s"blacklist_${p}_$adornmentString"
  }

  def extBlacklistName(p: String, adornment: Adornment): String = {
    s"ext_${blacklistName(p, adornment)}"
  }
}
