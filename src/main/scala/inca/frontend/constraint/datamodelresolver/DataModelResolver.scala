package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.DataModel
import inca.frontend.constraint.core.Module
import inca.runtime.context

trait DataModelResolver {
  def resolve(module: Module): context.DataModel =
    module.dataModels.foldLeft(new context.DataModel()) { case (res, dm) =>
      res ++ resolve(dm)
    }

  def resolve(dataModel: DataModel): context.DataModel =
    throw new IllegalArgumentException(
      s"Could not resolve this kind of language model ${dataModel}"
    )
}
