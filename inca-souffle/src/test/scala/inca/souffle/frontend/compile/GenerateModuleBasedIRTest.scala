package inca.souffle.frontend.compile

import inca.ir.*
import inca.ir.execution.{Relation2, Relation as Rel}
import inca.ir.extension.data.{DataDefinition, DataModuleEntry}
import inca.ir.extension.module.{Lowering, MainHint}
import inca.ir.extension.{aggregate, block, bool, data, datamatch, disjunction, not, set, tuple, arithmetic as arith}
import inca.ir.typing.Typechecker
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.souffle.syntax.ProgramContent.{Pragma, Rule}
import inca.souffle.syntax.Term.StringLit
import inca.souffle.syntax.TypeDeclConstraint.ADTType
import inca.souffle.syntax.{Atom, Term, Type, *}
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
    () => new not.Lowering {}
  ) // arith + string + data

  class Compiled(val ir: Module) extends CompiledUnit:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt
    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation


  def execute(prog: Program): Map[String, Rel] =
    val genIR = GenerateIR()
    val mod = genIR.compileProgram(prog, "SouffleProgram")
    /*println(prog)
    println()
    println()
    println()
    println(mod)*/

    val compiled = new Compiled(mod)
    compiled.setPipeline(pipeline)

    val engine = new Executor().instantiate(compiled)
    val rels = engine.readAll()
    rels.map { rel =>
      rel.name -> rel
    }.toMap

  test("Component") {
    val file = FileUtil.readFileFromResource("inca/souffle/Component.dl")
    val prog = Parser.parseSouffle(file)
    val genIR = GenerateModuleBasedIR()
    var mods = genIR.compileProgram(prog, "SouffleModule").map(m => m.name -> m).toMap
    val newM = mods.values.find(m => m.name.name == "SouffleModule").map(m => m.addHint(MainHint)).get
    mods += newM.name -> newM
    mods.values.foreach(println)

    var checker = typechecker()
    checker.checkProgram(mods.values.toSeq)
    checker.failOnWarnings()
    checker.failOnError()

    val linking = new Lowering {}
    val linked = linking.lower(mods.values.toSeq)

    checker = typechecker()
    checker.checkProgram(Seq(linked))
    checker.failOnWarnings()
    checker.failOnError()

    println(linked)
  }

  test("Component inheritance") {
    val file = FileUtil.readFileFromResource("inca/souffle/ComponentInheritance.dl")
    val prog = Parser.parseSouffle(file)
    val genIR = GenerateModuleBasedIR()

    var mods = genIR.compileProgram(prog, "SouffleModule").map(m => m.name -> m).toMap
    val newM = mods.values.find(m => m.name.name == "SouffleModule").map(m => m.addHint(MainHint)).get
    mods += newM.name -> newM
    mods.values.foreach(println)

    var checker = typechecker()
    checker.checkProgram(mods.values.toSeq)
    checker.failOnWarnings()
    checker.failOnError()

    val linking = new Lowering {}
    val linked = linking.lower(mods.values.toSeq)

    checker = typechecker()
    checker.checkProgram(Seq(linked))
    checker.failOnWarnings()
    checker.failOnError()

    println(linked)
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