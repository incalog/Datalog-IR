package inca.frontend.objectoriented.interpreter

import scala.collection.mutable
import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.ToolBox

trait ScalaInterpreter {
  private lazy val toolbox: ToolBox[universe.type] = currentMirror.mkToolBox()

  private var topLevelObject: universe.Symbol = _

  protected var imports: mutable.ListBuffer[meta.Import] = mutable.ListBuffer()

  def interp(code: String): Any = {
    val tree = toolbox.parse(code)
    toolbox.eval(tree)
  }
}
