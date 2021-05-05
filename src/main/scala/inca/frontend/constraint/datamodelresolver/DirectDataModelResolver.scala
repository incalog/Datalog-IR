package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.tree.{DirectDataModel, DataModel}
import inca.runtime.context

trait DirectDataModelResolver extends DataModelResolver {
  override def resolve(dataModel: DataModel): context.DataModel = dataModel match {
    case DirectDataModel(dm) => dm
    case _ => super.resolve(dataModel)
  }

}
