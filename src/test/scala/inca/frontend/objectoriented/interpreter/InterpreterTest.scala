package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.parser.Parser
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class InterpreterTest extends AnyFunSuite {
  val parser: Parser = new Parser {}
  val typechecker: Typechecker = new Typechecker {}

  def runProg(path: String, args: Seq[Any]): Value = {
    val code = FileUtil.readFile(path)
    val mod = Parser.parse(code)
    typechecker.typecheck(mod)
    val main = mod.classes.flatMap(_.methods.find(_.isMain)).head
    new Interpreter(mod).interp(main, args.map(ScalaValue))
  }

  test("Base") {
    val res1 = runProg("objectoriented/unittests/base/Base1.oinca", Seq())
    val res2 = runProg("objectoriented/unittests/base/Base2.oinca", Seq())
    val res3 = runProg("objectoriented/unittests/base/Base3.oinca", Seq())
    assertResult(43)(res1.asScala)
    assertResult(43)(res2.asScala)
    assertResult(43)(res3.asScala)
  }

  test("Fib") {
    val res = runProg("objectoriented/unittests/Fib.oinca", Seq(11))
    assertResult(89)(res.asScala)
  }
}
