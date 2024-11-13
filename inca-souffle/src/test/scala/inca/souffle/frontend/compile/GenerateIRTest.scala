package inca.souffle.frontend.compile

import inca.ir.*
import inca.ir.execution.{Relation2, Relation as Rel}
import inca.ir.extension.data.{DataDefinition, DataModuleEntry}
import inca.ir.extension.module.Lowering
import inca.ir.extension.{aggregate, block, bool, data, datamatch, disjunction, module, not, set, tuple, arithmetic as arith}
import inca.ir.optimize.AliasElimination
import inca.ir.typing.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.souffle.syntax.ProgramContent.{Pragma, Rule}
import inca.souffle.syntax.Term.StringLit
import inca.souffle.syntax.TypeDeclConstraint.ADTType
import inca.souffle.syntax.*
import inca.util.FileUtil
import inca.util.compileroptions.CompilerOptions
import inca.util.compileroptions.CompilerOptions.default
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class GenerateIRTest extends AnyFunSuite:
  def typechecker(): Typechecker = new Typechecker {}

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new module.Lowering {},
    // optimize
    () => new AliasElimination {}
  ) // arith + string + data

  case class Compiled(irModules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean, name: Name) extends CompiledUnit:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logLowerings = false
      opt.irLogging.logOptimizations = false
      opt

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation


  def execute(prog: Program): Map[String, Rel] =
    val progName = "SouffleProgram"
    val genIR = GenerateIR()
    val generateMods = genIR.compileProgram(prog, progName)

    // sort modules based on a topological order of dependencies
    val compiledProg = new CompiledProgram {
      override def irModules: Seq[Module] = generateMods

      override def createCompiledUnit(modules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean): CompiledUnit =
        Compiled(modules, otherUnits, isClosedWorld, modules.head.name)
    }
    compiledProg.setPipeline(pipeline)

    println()
    println("Generated:")
    generateMods.foreach(m => {
      println(); println(m)
    })

    compiledProg.compiledUnits.foreach { u =>
      println()
      println("After lowering:")
      println(u.lowered)
    }

    val engine = new Executor().instantiate(compiledProg.mainUnit)
    val rels = engine.readAll()
    //rels.foreach { r => println(r.asTable) }
    rels.map { rel =>
      rel.name -> rel
    }.toMap

  test("Component") {
    val file = FileUtil.readFileFromResource("inca/souffle/Component.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)

    assertResult(Set(42))(res("R").toSet)
    assertResult(Set(41, 42))(res("Q").toSet)
    assertResult(Set(41, 42))(res("config1$MagicNumber").toSet)
    assertResult(Set(41, 42))(res("config2$MagicNumber").toSet)
  }

  test("Component inheritance") {
    val file = FileUtil.readFileFromResource("inca/souffle/ComponentInheritance.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)

    assertResult(Set())(res("AbstractConfiguration$MagicNumber").toSet)
    assertResult(Set(42, 43))(res("S").toSet)
    assertResult(Set(42))(res("config1$MagicNumber").toSet)
    assertResult(Set(43))(res("config2$MagicNumber").toSet)
  }

  test("Nested components") {
    val file = FileUtil.readFileFromResource("inca/souffle/NestedComponents.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)

    assertResult(Set(41, 42))(res("S").toSet)
    assertResult(Set(41, 42))(res("b$Q").toSet)
    assertResult(Set(42))(res("b$a$R").toSet)
  }

  test("Nested components 2") {
    val file = FileUtil.readFileFromResource("inca/souffle/NestedComponents2.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)

    val pathSet = Set(
      ("a", "b"), ("b", "b"), ("c", "b"),
      ("a", "c"), ("b", "c"), ("c", "c"),
      ("a", "d"), ("b", "d"), ("c", "d")
    )

    assertResult(pathSet)(res("comp$innerComp$path").toSet)
    assertResult(pathSet)(res("comp$innerComp2$path").toSet)
    assertResult(Set(4, 5))(res("zero").toSet)
  }

  // TODO: Not supported
  /*test("Inherit Component Init") {
    val file = FileUtil.readFileFromResource("inca/souffle/InheritComponentInit.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)
    res.foreach(a => println(a._2.asTable))
    //assertResult("comp$innerComp$Succ(comp$innerComp$Zero())")(res("nats").entries.head.toString)
  }*/

  test("ADT") {
    val file = FileUtil.readFileFromResource("inca/souffle/ADT.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)
    assertResult("Succ(Zero())")(res("nats").entries.head.toString)
  }

  test("ADT - Component Inheritance") {
    val file = FileUtil.readFileFromResource("inca/souffle/ComponentInheritanceADT.dl")
    val prog = Parser.parseSouffle(file)
    val res = execute(prog)
    assertResult("comp$innerComp$Succ(comp$innerComp$Zero())")(res("nats").entries.head.toString)
  }