package inca.bddbddb.frontend.compile

import inca.bddbddb.syntax.Parser
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

class CompilerTest extends AnyFunSuite:
  test("Can compile PointsTo") {
    val content = FileUtil.readFileFromResource("inca/bddbddb/pa.datalog")
    val prog = Parser.parseModule(content)
    val generateIR = new GenerateIR
    val mod = generateIR.compileProgram(prog, "PointsTo")
    println(mod)
  }
