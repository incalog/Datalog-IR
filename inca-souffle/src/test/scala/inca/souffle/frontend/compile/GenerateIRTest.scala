package inca.souffle.frontend.compile

import inca.ir.*
import inca.ir.execution.{Relation2, Relation as Rel}
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.data.{DataDefinition, DataModuleEntry}
import inca.ir.extension.{aggregate, aggregateset, block, bool, data, datamatch, demand, disjunction, impure, not, set, tuple, arithmetic as arith}
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.souffle.backend.Executor
import inca.souffle.backend.compile.GenerateSouffle
import inca.souffle.syntax.ProgramContent.Pragma
import inca.souffle.syntax.Term.StringLit
import inca.souffle.syntax.{Atom, Term, Type, *}
import inca.util.compileroptions.CompilerOptions
import inca.util.compileroptions.CompilerOptions.default
import org.scalatest.funsuite.AnyFunSuite

class GenerateIRTest extends AnyFunSuite:
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new aggregateset.Lowering {},
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {},
    () => new demand.Lowering {},
    () => new tuple.Lowering {}
  ) // arith + string + data

  class Compiled(val ir: Module) extends CompiledModule:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt
    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
  
  test("Simple Program test") {
    val prog = Program(Seq(
      ProgramContent.RelationDecl(
        Seq("edge"),
        Seq(
          Attribute("n", Type.Symbol), Attribute("m", Type.Symbol)
        ),
        Seq(),
        None
      ),
      ProgramContent.Fact("edge", Seq(Term.StringLit("a"), Term.StringLit("b"))),
      ProgramContent.Fact("edge", Seq(Term.StringLit("b"), Term.StringLit("c"))),
      ProgramContent.Fact("edge", Seq(Term.StringLit("c"), Term.StringLit("b"))),
      ProgramContent.Fact("edge", Seq(Term.StringLit("c"), Term.StringLit("d"))),
      ProgramContent.RelationDecl(
        Seq("reachable"),
        Seq(
          Attribute("n", Type.Symbol), Attribute("m", Type.Symbol)
        ),
        Seq(),
        None
      ),
      ProgramContent.Directive(DirectiveQualifier.Output, QualifiedName(Seq("reachable")), Map()),
      ProgramContent.Rule(
        Seq(
          Atom.Call(QualifiedName(Seq("reachable")), Seq(Term.Var("x"), Term.Var("y")))
        ),
        Seq(
          Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y")))
        ),
        None
      ),
      ProgramContent.Rule(
        Seq(
          Atom.Call(QualifiedName(Seq("reachable")), Seq(Term.Var("x"), Term.Var("z")))
        ),
        Seq(
          Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y"))),
          Atom.Call(QualifiedName(Seq("reachable")), Seq(Term.Var("y"), Term.Var("z")))
        ),
        None
      )
    ))
    println(prog)
    
    val genIR = GenerateIR()
    val mod = genIR.compileProgram(prog, "SouffleProgram")
    println(mod)
    
    val compiled = new Compiled(mod)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val path = engine.read(Relation2("reachable", Seq("from", "to"), Seq()))
    println(path.asTable)    
  }
