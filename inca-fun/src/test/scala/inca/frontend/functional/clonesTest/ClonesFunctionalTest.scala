package inca.frontend.functional.clonesTest

import inca.frontend.functional.compile.CompiledFunctionalModule
import inca.frontend.functional.executor.FunctionalExecutor
import inca.frontend.functional.foreign
import inca.ir
//import inca.ir.execution.Relation
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{BaseIR, Language, Name, Module as IRModule}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{IntNum, TInt}
import ir.{Relation,Param,Eq,Var,Body}


class ClonesFunctionalTest extends AnyFunSuite{
  val exec: FunctionalExecutor = new FunctionalExecutor(inca.viatra.Executor)

  def performTest(path: String, expectedVNResult: IRModule, expectedExecResult: Any, argsExec: Seq[Any] = Seq(), functionName: String = "main"): Unit = {
    val code = FileUtil.readFileFromResource(path)
    val compiled = exec.compileFunction(code)
    compiled.setPipeline(CompiledFunctionalModule.pipeline)

    // TODO value numbering -> fill in missing expected results below
    val vnResult: IRModule = compiled.valueNumbering(compiled.lowered)  // TODO in the moment lowered twice: here and in Executor
    println("ValueNumbering Result: ")
    println(vnResult)
    assertResult(expectedVNResult)(vnResult)

    // still computes same result?
    val loadedOriginal = exec.loadFunction(compiled)
    val resOriginal = loadedOriginal.execute(functionName, Seq())
    assertResult(expectedExecResult)(resOriginal.entries.head)
    // assertResult(expectedExecResult)(exec.loadFunction(vnResult).execute("main", Seq()).entries.head)


  }

  test("simple redundant let") {
    val expected = IRModule(Name("Let"),
      Language(Set(new BaseIR{}, new data.IR{}, new string.IR{}, new aggregate.IR{}, new arithmetic.IR{})),
      Seq(
        Relation(Name("main"), 
          Seq(Param(Name("main_result$0"), TInt)), 
          Seq(Body(Seq(Eq(Var(Name("main_result$0")), IntNum(7))))))
      )
    )
    performTest("functional/clones/letRedundant.finca", expected , 7)
  }

  test("simple redundant let 2 (with input)") {
    performTest("functional/clones/letRedundant2.finca", ???, 1)
  }

  test("let with addition") {
    performTest("functional/clones/letAddition.finca", ???, 33)
  }

  test("let with addition (commutativity)") {
    performTest("functional/clones/letAddCommutativ.finca", ???, 33, Seq(1))
  }

  test("let with addition (associativity)") {
    performTest("functional/clones/letAddAssociative.finca", ???, 36, Seq(1))
  }



  test("if Condition with Add") {
    performTest("functional/clones/ifCondition.finca", ???, 7, Seq())
  }

  test("if Condition2 (with input)") {
    performTest("functional/clones/ifCondition2.finca", ???, 7, Seq(7))
  }

  test("if Condition3 (nested with input)") {
    performTest("functional/clones/ifCondition3.finca", ???, 2, Seq(1))
  }


  test("if Branches") {
    performTest("functional/clones/ifBranches.finca", ???, 1, Seq(1))
  }



}
