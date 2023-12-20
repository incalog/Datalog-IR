package inca.frontend.oodl.executor.unittests

import inca.frontend.oodl.compile.{CompiledOODLModule, GenerateScala}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorCaseClassTest extends AnyFunSuite:
  val options = CompilerOptions.fromResource("objectoriented/Options.ini")
  val exec: OODLExecutor = new OODLExecutor(inca.viatra.Executor)

  /** Case class */

  test("Case class") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/caseclass/CaseClass.oodl")

    val compiled = exec.compileOODL(code, options)

    // TEST
    /*println(code)
    println()
    println()
    val genScala = new GenerateScala()
    val scalaCode = genScala.transModule(compiled.typed)
    println(scalaCode)
    System.exit(1)*/

    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    val res = loaded.execute("main", Seq())
    assertResult(15)(res.entries.head)
  }

  test("Transitive closure") {
    val code = FileUtil.readFileFromResource("objectoriented/unittests/caseclass/TransitiveClosure.oodl")
    val compiled = exec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLModule.pipeline)
    val loaded = exec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())
    val setAdt = res.entries.head
    val query = Relation.from("Set$$TString_TString$$$enum", Seq("$set", "$elem"), Seq(Seq(setAdt, null)))
    res = loaded.engine.read(query).project(1, 3)
    val expectedResult = Set(
      ("A", "W"), ("Z", "Y"), ("Y", "W"), ("B", "C"),
      ("Y", "Z"), ("X", "Z"), ("Y", "Y"), ("Z", "Z"),
      ("X", "Y"), ("B", "W"), ("X", "X"), ("Z", "W"),
      ("Y", "X"), ("X", "W"), ("B", "A"), ("Z", "X")
    )
    assertResult(expectedResult)(res.toSet)
  }
