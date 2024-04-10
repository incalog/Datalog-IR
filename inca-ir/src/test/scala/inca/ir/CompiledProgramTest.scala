package inca.ir.CompiledProgram

import inca.ir.*
import inca.ir.CompiledProgram.CompiledProgram
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.typing.{BaseIRTypechecker, Typechecker}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.arithmetic

class CompiledProgramTest extends AnyFunSuite:

  def module(name: Name, relations: ModuleEntry*): Module =
    val typechecker = () => new BaseIRTypechecker with arithmetic.Typechecker {}
    val mod = Module(name, BaseIR.language, relations)
    //val checker = typechecker()
    //try checker.checkProgram(Seq(mod))
    //finally {
    //  println(mod)
    //  checker.getErrors.foreach(println)
    //}
    mod
  
  test("simple 2Module relation import") {
    case class TestCompiledModule(mod: Module) extends CompiledModule:
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def name: Name = mod.name
      override def sourceLocation: SourceLocation = mod.name
      override def ir: Module = mod

    val module1: Module = module("Module1", Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
      Eq(Var("x"), Var("y")))))),
      RelationExport("R", Seq(TInt, TInt)))
    val module2: Module = module("Module2", RelationImport("Q", Seq(TInt, TInt)),
      Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
        Call("Q", Seq(Var("a"), Var("b")))
      )))))

    val compiledModule1 = new TestCompiledModule(module1)
    val compiledModule2 = new TestCompiledModule(module2)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(new Link("Module1", "R", "Module2", "Q"))
      override val modules: Seq[CompiledModule] = Seq(new TestCompiledModule(module1), new TestCompiledModule(module2))
    
    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.intraTypecheck
    assert(TestCompiledProgram.linkedModule ==
      module("Module2", Relation("R_Module1", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), Var("y")))))),
        Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
          Call("R_Module1", Seq(Var("a"), Var("b")))))))
      )
    )
  }