package inca.backend.souffle

import inca.examples.functional.{Code, ControlDataFlow}
import inca.frontend.souffle.Syntax.{Name, cleanRuleName}
import inca.util.measurement.BenchmarkUtils.writeFile
import org.scalatest.funsuite.AnyFunSuite


class TestGenerateSouffle extends AnyFunSuite {

  val rootDir = "souffle-frontend/benchmark/generated"
  test("plus") {
    val compiled = CompiledFunctionalToSouffleModule(Code.plusModule)
    println(compiled.souffleSource)
  }

  test("plus main input") {
    import scala.meta._
    generateSouffle(rootDir + "/plus", Code.plusRealModule, "ext_input__main", Seq(q"Succ(Succ(Zero()))", q"Succ(Zero())"))
  }

  test("fib main input") {
    import scala.meta._
    generateSouffle(rootDir + "/fib", Code.fibModule, "ext_input__main", Seq(q"10"))
  }

  test("powerset dataflow analysis") {
    generateSouffle(s"${rootDir}/dataflow/ex4", ControlDataFlow.IntValuesModule, "ext_input__final_var", Seq(ControlDataFlow.exampleDataflow4))
  }

  def generateSouffle(dir: String, prog: String, extInput: String, input: Seq[meta.Term]): Unit = {
    val compiled = CompiledFunctionalToSouffleModule(prog)
    writeFile(s"${dir}/analysis.dl", compiled.souffleSource)
    // create fact files for each relation marked .input
    compiled.inputRelations.foreach { name =>
      writeFile(s"${dir}/${cleanRuleName(name)}.facts", "")
    }

    // fill fact files based on editscript
    val facts = compiled.generateFacts(input, Name(extInput))
    facts.foreach { case (name, relation) =>
      val relationString = relation.map(_.map(_.toString).mkString("\t")).mkString("\n")
      println(relationString)
      writeFile(s"${dir}/${cleanRuleName(name)}.facts", relationString)
    }
  }
}
