package inca.codeql.compile

import inca.codeql.syntax.{Parser, PredicateDecl, Program}
import inca.codeql.typecheck.Typechecker
import _root_.inca.ir.extension.*
import _root_.inca.ir.optimize
import _root_.inca.ir.optimize.Optimizer
import _root_.inca.ir.util.SourceLocation
import _root_.inca.ir.visitors.BaseIRVisitor
import _root_.inca.ir.{CompiledUnit, Module as IRModule, Name}

case class CompiledCodeQlUnit(program: Program, override val compilerOptions: CodeQlCompilerOptions) extends CompiledUnit:
  override val name: Name = Name("CodeQL")
  override val sourceLocation: SourceLocation = program
  override val isClosedWorld: Boolean = true
  override val otherUnits: Seq[CompiledUnit] = Seq.empty

  lazy val typed: Program =
    val typechecker = Typechecker()
    typechecker.checkProgram(program)
    messages ++= typechecker.getErrors
    messages ++= typechecker.getWarnings
    stopIfNeeded()
    program

  override lazy val irModules: Seq[IRModule] = Seq(GenerateIR(typed).compileProgram())

  def externalPredicate(name: Name): Option[PredicateDecl] = program.externalPredicates.find(_.name == name)

object CompiledCodeQlUnit:
  val SelectRelationName: Name = Name("__codeql_select")

  def fromSource(source: String, options: CodeQlCompilerOptions = CodeQlCompilerOptions.default): CompiledCodeQlUnit =
    val compiled = CompiledCodeQlUnit(Parser.parseProgram(source), options)
    compiled.setPipeline(pipeline)
    compiled.setOptimizationPipeline(optimizationPipeline)
    compiled

  val pipeline: List[() => BaseIRVisitor] = List(
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {},
    () => new optimize.IdentityCastElimination {},
    () => new optimize.AliasElimination {}
  )

  // IRConstantOptimizer can remove a class membership relation that remains
  // referenced by the negated guards used for dynamic member dispatch.
  val optimizationPipeline: List[() => Optimizer] = List.empty
