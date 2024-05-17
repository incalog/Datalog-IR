package inca.souffle.frontend.compile

import inca.ir
import inca.ir.extension.{block, bool, disjunction, module, not}
import inca.ir.{CompiledProgram, CompiledUnit, ExtensionalRelation, Module, Name, Param, Relation}
import inca.ir.util.SourceLocation
import inca.ir.visitors.{BaseIRVisitor, IRVisitor}
import inca.souffle.syntax.{DirectiveValue, Parser, Program}
import inca.util.compileroptions.CompilerOptions
import inca.ir.execution.{UnitRelation, Relation as ExecutionRelation}
import inca.ir.extension.arithmetic.{TDouble, TInt}

import scala.io.Source

case class CompiledSouffleUnit(name: Name, irModules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean, compilerOptions: CompilerOptions) extends CompiledUnit:
  override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation


case class CompiledSouffleProgram(name: Name, program: Program, compilerOptions: CompilerOptions = CompilerOptions.default) extends CompiledProgram {

  setPipeline(
    List(
      () => new bool.Lowering {},
      () => new block.Lowering {},
      () => new disjunction.Lowering {},
      () => new not.Lowering {},
      () => new module.Lowering {}
    )
  )

  def createCompiledUnit(modules: Seq[Module], otherUnits: Seq[CompiledUnit], isClosedWorld: Boolean): CompiledUnit =
    CompiledSouffleUnit(modules.head.name, modules, otherUnits, isClosedWorld, compilerOptions)

  lazy val irModules: Seq[Module] =
    val genIR = new GenerateModuleBasedIR
    genIR.compileProgram(program, name.name)

  private def loadEdbFactsFromFile(baseDir: String, attrs: Map[String, DirectiveValue]): Seq[Seq[String]] =
    val io = attrs.getOrElse("IO", DirectiveValue.StringLit("file"))
    io match
      case DirectiveValue.StringLit("file") =>
        val filename = attrs.get("filename") match
          case Some(DirectiveValue.StringLit(fn)) => fn
          case _ => throw new RuntimeException(s"Invalid or missing filename!")
        val delimiter = attrs.get("delimiter") match
          case Some(DirectiveValue.StringLit(d)) => d
          case _ => "\t"
        val file = Source.fromResource(s"$baseDir/$filename")
        val lines = file.getLines().map(_.split(delimiter).toSeq).toSeq
        file.close()
        lines
      case _ => throw new RuntimeException(s"Unsupported IO type: $io")

  /**
   * Get all relations marked as output in the souffle program
   * @return A sequence of relations used for output
   */
  lazy val outputRelations: Seq[ExecutionRelation] =
    var outputs: Seq[ExecutionRelation] = Seq()
    new IRVisitor {
      override def visitRelation(relation: Relation): Seq[Relation] =
        relation.getHint(SouffleOutputHint) match
          case Some(_) => outputs = outputs :+ UnitRelation(relation.name.name)
          case _ => // nothing
        super.visitRelation(relation)
    }.visitProgram(mainUnit.lowered)
    outputs

  /**
   * Load all facts marked as input in the souffle program
   * @param baseDir The base directory to load input files from
   *                (relative to the resources directory)
   * @return A sequence of relations used for edb inputs
   */
  def loadEdbInputs(baseDir: String): Seq[ExecutionRelation] =
    var inputs: Map[String, (Seq[Param], Map[String, DirectiveValue])] = Map()
    new IRVisitor {
      override def visitExtensionalRelation(relation: ExtensionalRelation): Seq[ExtensionalRelation] =
        relation.getHint[SouffleInputHint](SouffleInputHint) match
          case Some(SouffleInputHint(attrs)) => inputs += relation.name.name -> (relation.params, attrs)
          case _ => // nothing
        super.visitExtensionalRelation(relation)
    }.visitProgram(mainUnit.lowered)

    inputs.map { case (name, (params, attrs)) =>
      val tys = params.map(_.ty)
      ExecutionRelation.from(
        name,
        params.map(_.name.name),
        loadEdbFactsFromFile(baseDir, attrs).map {
          ss => ss.zip(tys).map {
            case (s, TInt) => s.toInt
            case (s, TDouble) => s.toFloat
            case (s, _) => s
          }
        }
      )
    }.toSeq
}

object CompiledSouffleProgram:
  def fromSource(name: Name, source: Source, compilerOptions: CompilerOptions = CompilerOptions.default): CompiledSouffleProgram =
    val content = source.getLines().mkString("\n")
    source.close()
    fromSourceCode(name, content, compilerOptions)

  def fromSourceCode(name: Name, source: String, compilerOptions: CompilerOptions = CompilerOptions.default): CompiledSouffleProgram =
    val program: Program = Parser.parseSouffle(source)
    new CompiledSouffleProgram(name, program, compilerOptions)
