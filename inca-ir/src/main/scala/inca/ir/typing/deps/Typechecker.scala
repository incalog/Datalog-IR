package inca.ir.typing.deps

import inca.ir.{Name, Relation}
import inca.ir.typing.BaseIRTypechecker

trait Typechecker extends BaseIRTypechecker:


  var varOrigins: Map[Name, FunctionalOrigins] = Map()

  def addOrigin(name: Name, origin: FunctionalOrigin): Unit =
    varOrigins.get(name) match
      case None => varOrigins += name -> FunctionalOrigins(Set(origin))
      case Some(origins) => varOrigins += name -> (origins + origin)

  override def checkRelation(relation: Relation): Unit =
    relation.getHint(FunctionalDependencyHint).foreach { case FunctionalDependencyHint(deps) =>
      for (dep <- deps) {
        val origin = FunctionalOrigin(dep.from.map(FunctionalOriginPart.Parameter(relation, _)).toSet)
        for (to <- dep.to)
          addOrigin(to.name, origin)
      }
    }
    super.checkRelation(relation)

