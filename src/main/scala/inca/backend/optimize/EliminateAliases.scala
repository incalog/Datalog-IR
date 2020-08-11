package inca.backend.optimize

import inca.backend.ir.GP._
import inca.backend.ir.Substitute

import scala.collection.immutable.MultiDict

object EliminateAliases extends Optimizer {


  override def optimizePattern(pat: Pattern): Seq[Pattern] = {
    val unsubstitutable = pat.params.map(_.name).toSet
    val newbodies = pat.bodies.map(eliminateAliasesInBody(_, unsubstitutable))
    Seq(Pattern(pat.vis, pat.name, pat.params, newbodies))
  }

  private def eliminateAliasesInBody(body: Body, unsubstitutable: Set[String]): Body = {
    // substMap tracks aliases
    var substMap: Map[Var, Var] = Map()
    def addAlias(from: Var, to: Var): Unit = {
      substMap = substMap.view.mapValues(v => if (v==from) to else v).toMap + (from -> to)
    }
    def addAliases(vars: Set[Var]): Boolean = {
      val svars = vars.map(subst)
      val (unsubstVars, substVars) = svars.partition(v => unsubstitutable.contains(v.name))
      if (unsubstVars.nonEmpty) {
        val base = unsubstVars.head
        substVars.foreach(v => addAlias(v, base))
        substVars.nonEmpty
      } else if (substVars.size >= 2) {
        val base = substVars.head
        substVars.tail.foreach(v => addAlias(v, base))
        true
      } else {
        false
      }
    }
    def subst(x: Var): Var = substMap.getOrElse(x, x)
    def substTerm(t: Term): Term = t match {
      case v: Var => substMap.getOrElse(v, v)
      case c: Constant => c
    }

    // which vars map to the same path
    var paths: MultiDict[(Var, Link), Var] = MultiDict()

    body.constraints.foreach {
      case Compare(EqComparator, t1, t2) =>
        (substTerm(t1), substTerm(t2)) match {
          case (v1: Var, v2: Var) if !unsubstitutable.contains(v2.name) =>
            addAlias(v2, v1)
          case (v1: Var, v2: Var) if !unsubstitutable.contains(v1.name) =>
            addAlias(v1, v2)
          case _ => // nothing
        }
      case Path(src:Var, trg:Var, link, _) =>
        val trgU = subst(trg)
        if (!unsubstitutable.contains(trgU.name))
          paths += (src, link) -> trgU
      case _ => // nothing
    }


    var changed = false
    do {
      changed = false
      paths = paths.mapSets { case ((src, link), trgs) => (subst(src), link) -> trgs }
      paths.sets.foreach { case (_, trgs) =>
        changed = addAliases(trgs)
      }
    } while (changed)

    val substBody = Substitute(subst).substBody(body)
    val dedup = substBody.constraints.distinct
    Body(dedup)
  }
}

