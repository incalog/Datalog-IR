package inca.frontend.constraint.datamodelresolver

import inca.frontend.constraint.core.tree.{DataModel, Module, NativeDataModel}
import inca.runtime.context

import java.lang.reflect.InvocationTargetException

trait NativeDataModelResolver extends DataModelResolver {
  import scala.reflect.runtime.{currentMirror, universe}
  import scala.tools.reflect.{ToolBox, ToolBoxError}

  private lazy val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  override def resolve(dataModel: DataModel): context.DataModel = dataModel match {
    case NativeDataModel(path) =>
      val code = toolbox.parse(path)
      try {
        toolbox.eval(code).asInstanceOf[context.DataModel]
      } catch {
        case t: InvocationTargetException =>
          throw new IllegalArgumentException(s"Could not resolve native data model ${path}")
      }
    case _ => super.resolve(dataModel)
  }
}
