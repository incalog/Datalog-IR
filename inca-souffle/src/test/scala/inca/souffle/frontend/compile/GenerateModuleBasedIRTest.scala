package inca.souffle.frontend.compile

import inca.ir.*
import inca.ir.execution.{Relation2, Relation as Rel}
import inca.ir.extension.data.{DataDefinition, DataModuleEntry}
import inca.ir.extension.module.Lowering
import inca.ir.extension.{module, aggregate, block, bool, data, datamatch, disjunction, not, set, tuple, arithmetic as arith}
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

class GenerateModuleBasedIRTest extends AnyFunSuite:
  def typechecker(): Typechecker = new Typechecker {}

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new module.Lowering {}
  ) // arith + string + data

  case class Compiled(irModules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean, name: Name) extends CompiledUnit:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt.irLogging.logOptimizations = true
      opt

    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation


  def execute(prog: Program): Map[String, Rel] =
    val progName = "SouffleProgram"
    val genIR = GenerateModuleBasedIR()
    val generateMods = genIR.compileProgram(prog, progName)
    /*println(prog)
    println()
    println()
    println()
    println(mod)*/

    // TODO: Figure out topological order and compile based on that
    val modsMap = generateMods.map(m => m.name -> m).toMap
    val newM = modsMap.values.find(m => m.name.name == progName).get
    val mods = modsMap.removed(newM.name).values.toSeq

    //println(newM)
    //println()
    //mods.foreach(m => {println(); println(m) } )

    val dep = Compiled(mods, Seq(), false, "Config")
    dep.setPipeline(pipeline)

    val compiled = Compiled(Seq(newM), Seq(dep), true, progName)
    compiled.setPipeline(pipeline)

    val engine = new Executor().instantiate(compiled)
    val rels = engine.readAll()
    rels.map { rel =>
      rel.name -> rel
    }.toMap

  test("Component") {
    val file = FileUtil.readFileFromResource("inca/souffle/Component.dl")
    val prog = Parser.parseSouffle(file)
    execute(prog).foreach {
      (_, r) => println(r.asTable)
    }
  }

  test("Component inheritance") {
    val file = FileUtil.readFileFromResource("inca/souffle/ComponentInheritance.dl")
    val prog = Parser.parseSouffle(file)
    execute(prog).foreach {
      (_, r) => println(r.asTable)
    }
  }

  test("Nested components") {
    val file = FileUtil.readFileFromResource("inca/souffle/NestedComponents.dl")
    val prog = Parser.parseSouffle(file)
    val genIR = GenerateModuleBasedIR()

    var mods = genIR.compileProgram(prog, "SouffleModule").map(m => m.name -> m).toMap
    mods.foreach(println)
  }

  test("Nested components 2") {
    val file = FileUtil.readFileFromResource("inca/souffle/NestedComponents2.dl")
    val prog = Parser.parseSouffle(file)
    val genIR = GenerateModuleBasedIR()

    var mods = genIR.compileProgram(prog, "SouffleModule").map(m => m.name -> m).toMap
    mods.foreach(println)
  }