package inca.frontend.datamodelresolver

import inca.frontend.core.tree.{DataModel, Module}
import inca.runtime.context

trait DataModelResolver {
  def resolve(module: Module): context.DataModel = module.dataModels.foldLeft(new context.DataModel()) { case (res, dm ) =>
    res ++ resolve(dm)
  }

  def resolve(dataModel: DataModel): context.DataModel =
    throw new IllegalArgumentException(s"Could not resolve this kind of language model ${dataModel}")
}