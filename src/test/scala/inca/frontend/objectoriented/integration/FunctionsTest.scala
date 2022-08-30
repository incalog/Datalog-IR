package inca.frontend.objectoriented.integration

import inca.frontend.objectoriented.compiler.ObjectOptions
import inca.frontend.objectoriented.executor.ObjectExecutor
import inca.util.FileUtil.readFile
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class FunctionsTest extends AnyFunSuite {


  test("Plus Example") {
    val code = readFile("objectoriented/unittests/Plus.oinca")
    val fun = ObjectExecutor.loadFunction(code)
    fun.execute("main", Seq(q"10"))
  }
}