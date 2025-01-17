package inca.frontend.functional.executor.controlflow

import inca.frontend.functional.compile.{CompiledFunctionalUnit, FunctionalCompilerOptions}
import inca.frontend.functional.executor.FunctionalExecutor
import inca.ir.execution.Relation
import inca.util.FileUtil
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite


class FunctionalViatraExecutorTest extends AnyFunSuite:
  val options = FunctionalCompilerOptions.fromResource("functional/Options.ini")
  val exec: FunctionalExecutor = new FunctionalExecutor(new Executor)

  test("Interval Analysis") {
    val code = FileUtil.readFileFromResource("functional/controlflow/Interval.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val setAdt = loaded.execute("mainFinalVar", Seq(prog1)).entries.head
    val query = Relation.from("Set$$TString_Val$$enum", Seq("$set", "$elem$0", "$elem$1"), Seq(Seq(setAdt, null, null)))
    val res = loaded.engine.read(query).project(1)
    assertResult(
      Set("(x,IntervalVal(TopInterval()))", "(y,IntervalVal(TopInterval()))")
    )(
      res.entries.toSet.map(_.toString)
    )
  }

  test("Control Flow Analysis") {
    val code = FileUtil.readFileFromResource("functional/controlflow/CFlow.finca")
    val compiled = exec.compileFunction(code, options)
    compiled.setPipeline(CompiledFunctionalUnit.pipeline)
    //compiled.setPostProcessingPipeline(CompiledFunctionalUnit.viatraPostProcessingPipeline)
    compiled.setOptimizationPipeline(CompiledFunctionalUnit.optimizationPipeline)
    val loaded = exec.loadFunction(compiled)
    val setAdt = loaded.execute("mainTransitiveFlow", Seq(prog1)).entries.head
    val query = Relation.from("Set$$Stm_Stm$$enum", Seq("$set", "$elem$0", "$elem$1"), Seq(Seq(setAdt, null, null)))
    val res = loaded.engine.read(query).project(1)
    assertResult(
      Set("(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2))))", "(Assign(x,Add(Var(x),Num(2))),Assign(x,Add(Var(x),Num(2))))", "(Assign(x,Num(2)),Assign(z,Num(12)))", "(Assign(x,Add(Var(x),Num(2))),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(x,Add(Var(x),Num(2))),Assign(y,Add(Var(x),Var(y))))", "(While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))),Assign(x,Add(Var(x),Num(2))))", "(Assign(y,Add(Var(x),Var(y))),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(y,Add(Var(x),Var(y))),Assign(z,Num(12)))", "(While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(y,Add(Var(x),Var(y))),Assign(y,Add(Var(x),Var(y))))", "(Assign(y,Num(2)),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(z,Num(12)),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(y,Add(Var(x),Var(y))),Assign(x,Add(Var(x),Num(2))))", "(Assign(y,Num(2)),Assign(z,Num(12)))", "(Assign(x,Num(2)),Assign(y,Add(Var(x),Var(y))))", "(Assign(x,Num(2)),While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))))", "(Assign(x,Add(Var(x),Num(2))),Assign(z,Num(12)))", "(Assign(y,Num(2)),Assign(y,Add(Var(x),Var(y))))", "(Assign(z,Num(12)),Assign(y,Add(Var(x),Var(y))))", "(Assign(z,Num(12)),Assign(z,Num(12)))", "(Assign(x,Num(2)),Assign(y,Num(2)))", "(Assign(x,Num(2)),Assign(x,Add(Var(x),Num(2))))", "(While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))),Assign(y,Add(Var(x),Var(y))))", "(Assign(y,Num(2)),Assign(x,Add(Var(x),Num(2))))", "(While(GreaterThan(Var(x),Num(1)),Sequence(Assign(y,Add(Var(x),Var(y))),Sequence(Assign(z,Num(12)),Assign(x,Add(Var(x),Num(2)))))),Assign(z,Num(12)))")
    )(
      res.entries.toSet.map(_.toString)
    )
  }