package inca.ir

import inca.ir.*
import inca.ir.CompiledProgram
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.typing.{BaseIRTypechecker, Typechecker}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.arithmetic
import inca.ir.extension.data.*
import inca.ir.typing.TypeErrorException

class CompiledProgramTest extends AnyFunSuite:

  def module(name: Name, relations: ModuleEntry*)(using language: Language): Module = Module(name, language, relations)

  case class TestCompiledModule(mod: Module) extends CompiledModule:
    override def compilerOptions: CompilerOptions = CompilerOptions.default
    override def name: Name = mod.name
    override def sourceLocation: SourceLocation = mod.name
    override def ir: Module = mod

  test("simple 2Module relation import") {
    implicit val language = BaseIR.language
    val module1: Module = module("Module1",
      Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Eq(Var("y"), IntNum(2))
      )))),
      RelationExport("R", Seq(TInt, TInt)))
    val module2: Module = module("Module2", RelationImport("Q", Seq(TInt, TInt)),
      Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
        Call("Q", Seq(Var("a"), Var("b")))
      )))))

    val compiledModule1 = new TestCompiledModule(module1)
    val compiledModule2 = new TestCompiledModule(module2)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(Link("Module1", "R", "Module2", "Q"))
      override val compiledModules: Seq[CompiledModule] = Seq(TestCompiledModule(module1), TestCompiledModule(module2))
    
    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
      module("Module2",
        Relation("Module1$R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )))),
        Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
          Call("Module1$R", Seq(Var("a"), Var("b")))
        )))),
      )
    )
  }

  test("simple 2Module relation import wrong linkset") {
    implicit val language = BaseIR.language
    assertThrows[IllegalArgumentException] {
      val module1: Module = module("Module1",
        Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )))),
        RelationExport("R", Seq(TInt, TInt)))
      val module2: Module = module("Module2", RelationImport("Q", Seq(TInt, TInt)),
        Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(
          Call("Q", Seq(Var("a"), Var("b")))
        )))))

      val compiledModule1 = TestCompiledModule(module1)
      val compiledModule2 = TestCompiledModule(module2)

      class testCompiledProgram extends CompiledProgram:
        override val linkSet: Seq[Link] = Seq(Link("Module1", "F", "Module2", "Q"))
        override val compiledModules: Seq[CompiledModule] = Seq(TestCompiledModule(module1), new TestCompiledModule(module2))
      
      val TestCompiledProgram = new testCompiledProgram

      TestCompiledProgram.validateLinkSet()
    }
  }

  test("simple 2Module relation import wrong types") {
    implicit val language = BaseIR.language
    val module1: Module = module("Module1",
      Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Eq(Var("y"), IntNum(2))
      )))),
      RelationExport("R", Seq(TInt, TInt)))
    val module2: Module = module("Module2", RelationImport("Q", Seq(TNothing, TNothing)),
      Relation("T", Seq(Param("a", TNothing), Param("b", TNothing)), Seq(Body(Seq(
        Call("Q", Seq(Var("a"), Var("b")))
      )))))

    val compiledModule1 = TestCompiledModule(module1)
    val compiledModule2 = TestCompiledModule(module2)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(Link("Module1", "R", "Module2", "Q"))
      override val compiledModules: Seq[CompiledModule] = Seq(TestCompiledModule(module1), new TestCompiledModule(module2))
    
    val TestCompiledProgram = new testCompiledProgram

    assertThrows[TypeErrorException](
      TestCompiledProgram.validateLinkSet()
    )
  }

  test("2Module multiple relation import") {
    implicit val language = BaseIR.language
    val module1: Module = module("Module1",
      Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Eq(Var("y"), IntNum(2))
      )))),
      RelationExport("R", Seq(TInt, TInt)),
      Relation("S", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
      RelationExport("S", Seq(TInt)))

    val module2: Module = module("Module2",
      RelationImport("Q", Seq(TInt, TInt)),
      RelationImport("W", Seq(TInt)),
      Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(Call("Q", Seq(Var("a"), Var("b"))))))),
      Relation("U", Seq(Param("a", TInt)), Seq(Body(Seq(Call("W", Seq(Var("a"))))))))

    val compiledModule1 = TestCompiledModule(module1)
    val compiledModule2 = TestCompiledModule(module2)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(
        Link("Module1", "R", "Module2", "Q"),
        Link("Module1", "S", "Module2", "W")
      )
      override val compiledModules: Seq[CompiledModule] = Seq(compiledModule1, compiledModule2)

    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
      module("Module2",
        Relation("Module1$R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )))),
        Relation("Module1$S", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
        Relation("T", Seq(Param("a", TInt), Param("b", TInt)), Seq(Body(Seq(Call("Module1$R", Seq(Var("a"), Var("b"))))))),
        Relation("U", Seq(Param("a", TInt)), Seq(Body(Seq(Call("Module1$S", Seq(Var("a")))))))
      )
    )
  }

  test("multi-module relation import") {
    implicit val language = BaseIR.language
    case class TestCompiledModule(mod: Module) extends CompiledModule:
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def name: Name = mod.name
      override def sourceLocation: SourceLocation = mod.name
      override def ir: Module = mod

    val moduleC: Module = module("ModuleC",
      Relation("CR1", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Eq(Var("y"), IntNum(2))
      )))),
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

    val compiledModuleC = TestCompiledModule(moduleC)
    val compiledModuleB = TestCompiledModule(moduleB)
    val compiledModuleA = TestCompiledModule(moduleA)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(
        Link("ModuleC", "CR2", "ModuleB", "BR2"),
        Link("ModuleB", "BR", "ModuleA", "ABR"),
        Link("ModuleC", "CR1", "ModuleA", "AR1")
      )
      override val compiledModules: Seq[CompiledModule] = Seq(compiledModuleC, compiledModuleB, compiledModuleA)

    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
      module("ModuleA",
        Relation("ModuleC$CR1", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )))),
        Relation("ModuleC$CR2", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
        Relation("ModuleB$BR", Seq(Param("z", TInt)), Seq(Body(Seq(Call("ModuleC$CR2", Seq(Var("z"))))))),
        Relation("AR", Seq(Param("w", TInt), Param("v", TInt)), Seq(Body(Seq(Call("ModuleC$CR1", Seq(Var("w"), Var("v"))), Call("ModuleB$BR", Seq(Var("w")))))))
      )
    )
  }

  test("Module Data import") {
    implicit val language = inca.ir.extension.data.IR.language
    val module1: Module = module("Module1",
      DataDefinition("testdata"), 
      CaseDefinition("addition", Seq(TInt, TInt), TData("testdata")), 
      DataDefinitionExport("testdata"), 
      CaseDefinitionExport("addition", Seq(TInt, TInt), TData("testdata")))

    val module2: Module = module("Module2",
      DataDefinitionImport("testdata2"), 
      CaseDefinitionImport("addition2", Seq(TInt, TInt), TData("testdata2")), 
      Relation("Bind", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Eq(Var("x"), IntNum(1)),
        Eq(Var("y"), IntNum(2))
      )))),
      Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
        Call("Bind", Seq(Var("x"), Var("y"))),
        Eq(Construct("addition2", Seq(Var("x"), Var("x"))), Construct("addition2", Seq(Var("y"), Var("y"))))
      ))))
    )

    val compiledModule1 = TestCompiledModule(module1)
    val compiledModule2 = TestCompiledModule(module2)
  
    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(
        Link("Module1", "testdata", "Module2", "testdata2"),
        Link("Module1", "addition", "Module2", "addition2")
      )
      override val compiledModules: Seq[CompiledModule] = Seq(compiledModule1, compiledModule2)

    val TestCompiledProgram = new testCompiledProgram


    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
      module("Module2",
        DataDefinition("Module1$testdata"),
        CaseDefinition("Module1$addition", Seq(TInt, TInt), TData("Module1$testdata")),
        Relation("Bind", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )))),
        Relation("R", Seq(Param("x", TInt), Param("y", TInt)), Seq(Body(Seq(
          Call("Bind", Seq(Var("x"), Var("y"))), Eq(Construct("Module1$addition", Seq(Var("x"), Var("x"))), Construct("Module1$addition", Seq(Var("y"), Var("y"))))
        )))),
      )
    )
  }

  test("Cycle-Import Test") {
    implicit val language = BaseIR.language
    case class TestCompiledModule(mod: Module) extends CompiledModule:
      override def compilerOptions: CompilerOptions = CompilerOptions.default
      override def name: Name = mod.name
      override def sourceLocation: SourceLocation = mod.name
      override def ir: Module = mod

    val module1: Module = module("Module1",
      RelationImport("Q1", Seq(TInt)),
      RelationExport("R1", Seq(TInt)),
      RelationExport("L1", Seq(TInt)),
      Relation("R1", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
      Relation("L1", Seq(Param("a", TInt)), Seq(Body(Seq(Call("Q1", Seq(Var("a")))))))
    )

    val module2: Module = module("Module2",
      RelationImport("R2", Seq(TInt)),
      RelationExport("Q2", Seq(TInt)),
      RelationExport("T2", Seq(TInt)),
      Relation("Q2", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
      Relation("T2", Seq(Param("a", TInt)), Seq(Body(Seq(Call("R2", Seq(Var("a")))))))
    )

    val module3: Module = module("Module3",
      RelationImport("L3", Seq(TInt)),
      RelationImport("T3", Seq(TInt)),
      Relation("X3", Seq(Param("a", TInt)), Seq(Body(Seq(Call("L3", Seq(Var("a"))))))),
      Relation("Y3", Seq(Param("x", TInt)), Seq(Body(Seq(Call("T3", Seq(Var("x")))))))
    )

    val compiledModule1 = TestCompiledModule(module1)
    val compiledModule2 = TestCompiledModule(module2)
    val compiledModule3 = TestCompiledModule(module3)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(
        Link("Module1", "R1", "Module2", "R2"),
        Link("Module1", "L1", "Module3", "L3"),

        Link("Module2", "Q2", "Module1", "Q1"),
        Link("Module2", "T2", "Module3", "T3"),
      )

      override val compiledModules: Seq[CompiledModule] = Seq(compiledModule1, compiledModule2, compiledModule3)

    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
      module("Module3",
        Relation("Module1$R1", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
        Relation("Module1$L1", Seq(Param("a", TInt)), Seq(Body(Seq(Call("Module2$Q2", Seq(Var("a"))))))),
        Relation("Module2$Q2", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
        Relation("Module2$T2", Seq(Param("a", TInt)), Seq(Body(Seq(Call("Module1$R1", Seq(Var("a"))))))),
        Relation("X3", Seq(Param("a", TInt)), Seq(Body(Seq(Call("Module1$L1", Seq(Var("a"))))))),
        Relation("Y3", Seq(Param("x", TInt)), Seq(Body(Seq(Call("Module2$T2", Seq(Var("x")))))))
      )
    )
  }

  test("2-Level import") {
    implicit val language = BaseIR.language
    case class TestCompiledModule(mod: Module) extends CompiledModule:
      override def compilerOptions: CompilerOptions = CompilerOptions.default

      override def name: Name = mod.name

      override def sourceLocation: SourceLocation = mod.name

      override def ir: Module = mod

    val module1: Module = module("Module1",
      RelationExport("R1", Seq(TInt)),
      Relation("R1", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
    )

    val module2: Module = module("Module2",
      RelationImport("R2", Seq(TInt)),
      RelationExport("R2", Seq(TInt)),
    )

    val module3: Module = module("Module3",
      RelationImport("R3", Seq(TInt)),
      Relation("Test", Seq(Param("x", TInt)), Seq(Body(Seq(Call("R3", Seq(Var("x")))))))
    )

    val compiledModule1 = TestCompiledModule(module1)
    val compiledModule2 = TestCompiledModule(module2)
    val compiledModule3 = TestCompiledModule(module3)

    class testCompiledProgram extends CompiledProgram:
      override val linkSet = Seq(
        Link("Module1", "R1", "Module2", "R2"),
        Link("Module2", "R2", "Module3", "R3")
      )

      override val compiledModules: Seq[CompiledModule] = Seq(compiledModule1, compiledModule2, compiledModule3)

    val TestCompiledProgram = new testCompiledProgram

    TestCompiledProgram.validateLinkSet()
    assert(TestCompiledProgram.linkedModule ==
           module("Module3",
             Relation("Module1$R1", Seq(Param("x", TInt)), Seq(Body(Seq(Eq(Var("x"), IntNum(0)))))),
             Relation("Test", Seq(Param("x", TInt)), Seq(Body(Seq(Call("Module1$R1", Seq(Var("x")))))))
           )
    )
  }