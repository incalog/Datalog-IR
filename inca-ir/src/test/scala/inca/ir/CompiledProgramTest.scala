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
  
  case class TestCompiledModule(mod: Module) extends CompiledModule:
    override def compilerOptions: CompilerOptions = CompilerOptions.default
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = mod.name
    override def ir: Module = mod

  test("simple 2Module relation import") {
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

  test("2Module multiple relation import") {
   val module1: Module = module("Module1",
     Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(Eq(Var("x"), Var("y")))))),
     RelationExport("R", Seq(TInt, TInt)),
     Relation("S", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
     RelationExport("S", Seq(TInt)))

   val module2: Module = module("Module2",
     RelationImport("Q", Seq(TInt, TInt)),
     RelationImport("W", Seq(TInt)),
     Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(Call("Q", Seq(Var("a"), Var("b"))))))),
     Relation("U", Seq(Param("a", TInt)), Seq(Body(Seq(Call("W", Seq(Var("a"))))))))

   val compiledModule1 = new TestCompiledModule(module1)
   val compiledModule2 = new TestCompiledModule(module2)

   class testCompiledProgram extends CompiledProgram:
     override val linkSet = Seq(
       new Link("Module1", "R", "Module2", "Q"),
       new Link("Module1", "S", "Module2", "W")
     )
     override val modules: Seq[CompiledModule] = Seq(compiledModule1, compiledModule2)

   val TestCompiledProgram = new testCompiledProgram

   TestCompiledProgram.intraTypecheck
   assert(TestCompiledProgram.linkedModule ==
     module("Module2",
       Relation("R_Module1", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(Eq(Var("x"), Var("y")))))),
       Relation("S_Module1", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
       Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(Call("R_Module1", Seq(Var("a"), Var("b"))))))),
       Relation("U", Seq(Param("a", TInt)), Seq(Body(Seq(Call("S_Module1", Seq(Var("a")))))))
     )
   )
  }

  test("multi-module relation import") {
   case class TestCompiledModule(mod: Module) extends CompiledModule:
     override def compilerOptions: CompilerOptions = CompilerOptions.default
     override def name: Name = mod.name
     override def sourceLocation: SourceLocation = mod.name
     override def ir: Module = mod

   val moduleC: Module = module("ModuleC",
     Relation("CR1", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(Eq(Var("x"), Var("y")))))),
     RelationExport("CR1", Seq(TInt, TInt)),
     Relation("CR2", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
     RelationExport("CR2", Seq(TInt))
   )

   val moduleB: Module = module("ModuleB",
     RelationImport("BR2", Seq(TInt)),
     Relation("BR", Seq(Param("z", TInt)), Seq(Body(Seq(Call("BR2", Seq(Var("z"))))))),
     RelationExport("BR", Seq(TInt))
   )

   val moduleA: Module = module("ModuleA",
     RelationImport("AR1", Seq(TInt, TInt)),
     RelationImport("ABR", Seq(TInt)),
     Relation("AR", Seq(Param("w", TInt), Param("v", TInt)), Seq(Body(Seq(Call("AR1", Seq(Var("w"), Var("v"))), Call("ABR", Seq(Var("w")))))))
   )

   val compiledModuleC = new TestCompiledModule(moduleC)
   val compiledModuleB = new TestCompiledModule(moduleB)
   val compiledModuleA = new TestCompiledModule(moduleA)

   class testCompiledProgram extends CompiledProgram:
     override val linkSet = Seq(
       new Link("ModuleC", "CR1", "ModuleA", "AR1"),
       new Link("ModuleC", "CR2", "ModuleB", "BR2"),
       new Link("ModuleB", "BR", "ModuleA", "ABR")
     )
     override val modules: Seq[CompiledModule] = Seq(compiledModuleC, compiledModuleB, compiledModuleA)

   val TestCompiledProgram = new testCompiledProgram

   TestCompiledProgram.intraTypecheck
   assert(TestCompiledProgram.linkedModule ==
     module("ModuleA",

       Relation("CR1_ModuleC", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(Eq(Var("x"), Var("y")))))),
       Relation("CR2_ModuleC", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
       Relation("BR_ModuleB", Seq(Param("z", TInt)), Seq(Body(Seq(Call("CR2_ModuleC", Seq(Var("z"))))))),
       Relation("AR", Seq(Param("w", TInt), Param("v", TInt)), Seq(Body(Seq(Call("CR1_ModuleC", Seq(Var("w"), Var("v"))), Call("BR_ModuleB", Seq(Var("w")))))))
    )
  )
  }