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
import inca.souffle.syntax.ProgramContent.{Pragma, Rule}
import inca.souffle.syntax.Term.StringLit
import inca.souffle.syntax.TypeDeclConstraint.ADTType
import inca.souffle.syntax.{Atom, ProgramContent, Term, Type, *}
import inca.util.compileroptions.CompilerOptions
import inca.util.compileroptions.CompilerOptions.default
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

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


  def execute(prog: Program): Map[String, Rel] =
    val genIR = GenerateIR()
    val mod = genIR.compileProgram(prog, "SouffleProgram")
    println(mod)

    val compiled = new Compiled(mod)
    val engine = new inca.viatra.Executor().instantiate(compiled)
    val rels = engine.readAll()
    rels.map { rel =>
      rel.name -> rel
    }.toMap



  test("compile path") {
    val prog = Program(Seq(
      ProgramContent.RelationDecl(
        Seq("edge"),
        Seq(
          Attribute("n", Type.Symbol), Attribute("m", Type.Symbol)
        ),
        Seq(),
        None
      ),
      ProgramContent.Fact(QualifiedName(Seq("edge")), Seq(Term.StringLit("a"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("edge")), Seq(Term.StringLit("b"), Term.StringLit("c"))),
      ProgramContent.Fact(QualifiedName(Seq("edge")), Seq(Term.StringLit("c"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("edge")), Seq(Term.StringLit("c"), Term.StringLit("d"))),
      ProgramContent.RelationDecl(
        Seq("path"),
        Seq(
          Attribute("n", Type.Symbol), Attribute("m", Type.Symbol)
        ),
        Seq(),
        None
      ),
      ProgramContent.Directive(DirectiveQualifier.Output, QualifiedName(Seq("path")), Map()),
      ProgramContent.Rule(
        Seq(
          Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("y")))
        ),
        Seq(
          Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y")))
        ),
        None
      ),
      ProgramContent.Rule(
        Seq(
          Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("z")))
        ),
        Seq(
          Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y"))),
          Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("y"), Term.Var("z")))
        ),
        None
      )
    ))
    println(prog)
    val path = execute(prog)("path")
    
    println(path.asTable)
  }

  test("component test") {
    val prog = Program(
      Seq(
      ProgramContent.ComponentDecl(ComponentType("Component", Seq()), Seq(), Seq(
        ProgramContent.TypeDecl("Base", TypeDeclConstraint.EqType(Type.Symbol)),
        ProgramContent.RelationDecl(Seq("edge"), Seq(
          Attribute("x", Type.Name(QualifiedName(Seq("Base")))),
          Attribute("y", Type.Name(QualifiedName(Seq("Base"))))
        ), Seq(), None),
        ProgramContent.RelationDecl(Seq("path"), Seq(
          Attribute("x", Type.Name(QualifiedName(Seq("Base")))),
          Attribute("y", Type.Name(QualifiedName(Seq("Base"))))
        ), Seq(), None),
        ProgramContent.Rule(
          Seq(Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("y")))),
          Seq(Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y")))),
          None
        ),
        ProgramContent.Rule(
          Seq(Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("y")))),
          Seq(
            Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("z"))),
            Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("z"), Term.Var("y")))
          ),
          None
        )
      )),
      ProgramContent.ComponentInit("comp", ComponentType("Component", Seq())),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("a"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("b"), Term.StringLit("c"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("d"))),
      ProgramContent.Directive(DirectiveQualifier.Output, QualifiedName(Seq("comp", "path")), Map())
    ))

    println(prog)
    val path = execute(prog)("comp$path")

    println(path.asTable)
  }

  test("adt test") {
    val prog = Program(Seq(
      ProgramContent.TypeDecl("Nat", ADTType(Seq(
        ADTConstructor("Zero", Seq()),
        ADTConstructor("Succ", Seq(Attribute("pred", Type.Name(QualifiedName(Seq("Nat")))))),
      ))),
      ProgramContent.RelationDecl(
        Seq("nats"),
        Seq(Attribute("n", Type.Name(QualifiedName(Seq("Nat"))))),
        Seq(), None
      ),
      ProgramContent.Rule(
        Seq(Atom.Call(QualifiedName(Seq("nats")), Seq(Term.Var("n")))),
        Seq(Atom.Equal(Term.Var("n"), Term.Constr("Succ", Seq(Term.Constr("Zero", Seq()))))),
        None
      )
    ))

    println(prog)
    val path = execute(prog)("comp$path")

    println(path.asTable)
  }