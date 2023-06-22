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

  def interpClosure(code: String, args: Any*): Any = {
    val closure = interp(code)
    callClosure(closure, args)
  }

  private def callClosure(closure: Any, args: Seq[Any]): Any = {
    closure match {
      case c: Function0[Any] => c()
      case c: Function1[Any, Any] => c(args(0))
      case c: Function2[Any, Any, Any] => c(args(0), args(1))
      case c: Function3[Any, Any, Any, Any] => c(args(0), args(1), args(2))
      case c: Function4[Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3))
      case c: Function5[Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4))
      case c: Function6[Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5))
      case c: Function7[Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6))
      case c: Function8[Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7))
      case c: Function9[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8))
      case c: Function10[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9))
      case c: Function11[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10))
      case c: Function12[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11))
      case c: Function13[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12))
      case c: Function14[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13))
      case c: Function15[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14))
      case c: Function16[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15))
      case c: Function17[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16))
      case c: Function18[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17))
      case c: Function19[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18))
      case c: Function20[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19))
      case c: Function21[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19), args(20))
      case c: Function22[Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any] => c(args(0), args(1), args(2), args(3), args(4), args(5), args(6), args(7), args(8), args(9), args(10), args(11), args(12), args(13), args(14), args(15), args(16), args(17), args(18), args(19), args(20), args(21))
      case _ => throw new IllegalArgumentException("Unsupported closure type")
    }
  }
}
