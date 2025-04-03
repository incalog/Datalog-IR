package inca.frontend.oodl.executor.casestudies

import inca.frontend.oodl.compile.{CompiledOODLUnit, OODLCompilerOptions}
import inca.frontend.oodl.executor.{OODLExecutor, TypeCastException}
import inca.ir.execution.{Relation, UnitRelation}
import inca.ir.optimize.AbstractEdbConfig
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory
import org.scalatest.funsuite.AnyFunSuite

class OODLViatraExecutorCaseStudyTest extends AnyFunSuite:
  val options = OODLCompilerOptions.fromResource("objectoriented/Options.ini")
  // Timely algorithm fails without a message
  val exec: OODLExecutor = new OODLExecutor(new Executor)

  test("DependencyAnalysis") {
    val endNode = 50
    val step = 10

    val code = FileUtil.readFileFromResource("objectoriented/casestudies/DependencyAnalysis.oodl")
    val unit = exec.compileOODL(code, options)
    unit.setPipeline(CompiledOODLUnit.pipeline)
    unit.setOptimizationPipeline(unit.optimizationPipeline)
    val loaded = exec.loadOODL(unit)
    var res = loaded.execute("main", Seq(endNode, step))

    val setAdt = res.entries.head
    val query = Relation.from("Set$TString$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 2)

    assertResult(0.to(endNode).map("a" + _).toSet)(res.toSet)
  }

  test("ControlFlowGraph") {
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/CfgVisitor.oodl")
    val unit = exec.compileOODL(code, options)
    unit.setPipeline(unit.pipeline)
    unit.setOptimizationPipeline(unit.optimizationPipeline)
    val loaded = exec.loadOODL(unit)
    //var res = loaded.execute("main", Seq())
    //val setAdt = res.entries.head
    //val query = Relation.from("Set$$TString_TString$$enum", Seq("$set"), Seq(Seq(setAdt)))

    loaded.execute("main", Seq())
    val res = loaded.engine.read(UnitRelation("Set$$TString_TString$$enum"))

    val expectedRes = Set(
      ("VarDef", "While"), ("Assign", "While"), ("While", "Assign"),
      ("VarDef", "VarDef"), ("Assign", "Assign")
    )

    //println(diff.toDouble / 1000 / 1000 / 1000)
    assertResult(expectedRes)(res.toSet)
  }

  // This compiles but is way slower than the old case study... why ?
  // Ideas, constructing a lot of objects is expensive, since we duplicate a lot of constructor calls
  test("Flow sensitive Sign Analysis") {
    val dRedExec: OODLExecutor = new OODLExecutor(new Executor(DRedReteBackendFactory.INSTANCE))
    val code = FileUtil.readFileFromResource("objectoriented/casestudies/FlowSensitiveSignAnalysis.oodl")
    val compiled = dRedExec.compileOODL(code, options)
    compiled.setPipeline(CompiledOODLUnit.pipeline)
    // FIXME: Using AbstractEdbConfig will not replace impurity counter variables with constants.
    //  Somehow replacing them makes the program unexecutable slow. Maybe because some sets that
    //  contain objects are also constant now?
    compiled.setOptimizationPipeline(compiled.createOptimizationPipeline(false, AbstractEdbConfig.default))
    compiled.setPostProcessingPipeline(compiled.viatraPostProcessingPipeline)
    val loaded = dRedExec.loadOODL(compiled)
    var res = loaded.execute("main", Seq())

    val setAdt = res.entries.head
    val query = Relation.from("Set$$TString_TString_TString$$enum", Seq("$set"), Seq(Seq(setAdt)))
    res = loaded.engine.read(query).project(1, 4)

    val expectedRes = Set(
      ("ValDef(n, Num(5))", "acc", "Bot"),
      ("ValDef(acc, Num(1))", "acc", "Pos"),
      ("ValDef(acc, Num(1))", "n", "Pos"),
      ("ValDef(n, Num(5))", "n", "Pos")
    )
    assertResult(expectedRes)(res.toSet)

    //println(diff.toDouble / 1000 / 1000 / 1000)
    //assertResult(expectedRes)(res.toSet)
  }