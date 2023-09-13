package inca.frontend.objectoriented.datalog

import inca.compiler.{CompiledModule, Compiler}
import inca.compiler.{CompiledModule, Compiler}
import inca.frontend.objectoriented
import inca.frontend.objectoriented.compiler.{CompiledObjectModule, ObjectOptions}
import inca.frontend.objectoriented.lowering.{GenerateDataModel, GenerateDatalog}
import inca.frontend.objectoriented.lowering.preprocessing.{AddMissingDefinitions, Defunctionalize, StaticSingleAssignment}
import inca.frontend.objectoriented.typechecker.Typechecker
import inca.util.FileUtil
import org.scalatest.funsuite.AnyFunSuite

import scala.meta.XtensionQuasiquoteTerm

class OODatalogTest extends AnyFunSuite {
  lazy val addModule: CompiledObjectModule = Compiler.compileObject(
    """module SetTest

        class MCounter extends BalancedMonotone[Int, Int] {
          var st : Int = 0
          @static def init() : Int = { return 0 }
          @static def join(a : Int, b : Int) : Int = { return a + b }
          def lift(a : Int) : Int = {return this.st + a}
          def result() : Int = { return this.st }
        }

        class Example {
          @main
          def main() : Int = {
            val t : MCounter = new MCounter()
            t += 2
            return t.result()
          }
         }



    """, ObjectOptions())

  test("Add Example") {
    val datalog = new ObjectOrientedDatalog(addModule)
    val resRel = datalog.run("Example", "main")

//    println(resRel)
    datalog.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }

    println()
    println()

    //    val resRel2 = datalog.run("A", "add", q"1", q"3")
    //    println(resRel2)
    //    datalog.readAll.foreach { rel =>
    //      println()
    //      println(rel.asTable)
    //    }
    //
    //    assert(resRel.toSet.head == 3)
    //    assert(resRel2.toSet.head == 4)
    //  }
  }

  private val code = FileUtil.readFile("objectoriented/unittests/monotypes/Case1.oinca")
  test("Example 1") {
    val par = objectoriented.parser.Parser.parse(code)
    val parsed = AddMissingDefinitions.transformModule(par)
    val typer: Typechecker = new Typechecker {}
    typer.typecheck(parsed)
    val ssa = StaticSingleAssignment.transformModule(parsed)
    typer.typecheck(ssa)
    val dataModel = new GenerateDataModel(ssa)
    val dataModelTrans = dataModel.transModule()
    println("dataModel ", dataModel.toString)
    println("dataModelTrans ", dataModelTrans.toString)
    val core = Defunctionalize.transformModule(ssa, dataModel.transModule())
    typer.typecheck(core)
    println(core)
    val codegen = new GenerateDatalog(parsed, core).transModule()
    println("codegen\n", codegen)
  }

  test("Example 2") {
    val parsed = objectoriented.parser.Parser.parse(code)
    val module = CompiledObjectModule(parsed, ObjectOptions())
    val datalog = new ObjectOrientedDatalog(module)
    val resRel = datalog.run("Example", "main")
    datalog.readAll.foreach { rel =>
      println()
      println(rel.asTable)
    }
  }
}

