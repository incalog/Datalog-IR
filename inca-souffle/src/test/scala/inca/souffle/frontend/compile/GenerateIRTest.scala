package inca.souffle.frontend.compile

import inca.ir.*
import inca.ir.execution.{Relation as Rel}
import inca.ir.extension.{block, bool, datamatch, disjunction, not, set}
import inca.ir.optimize.IdentityCastElimination
import inca.ir.util.SourceLocation
import inca.ir.visitors.BaseIRVisitor
import inca.souffle.syntax.ProgramContent.Rule
import inca.souffle.syntax.Term.StringLit
import inca.souffle.syntax.TypeDeclConstraint.ADTType
import inca.souffle.syntax.{Atom, ProgramContent, Term, Type, *}
import inca.util.compileroptions.CompilerOptions
import inca.util.compileroptions.CompilerOptions.default
import inca.viatra.backend.Executor
import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

class GenerateIRTest extends AnyFunSuite:
  val pipeline: List[() => BaseIRVisitor] = List(
    () => new set.Lowering {},
    () => new bool.Lowering {},
    () => new datamatch.Lowering {},
    () => new block.Lowering {},
    () => new disjunction.Lowering {},
    () => new not.Lowering {}
  ) // arith + string + data

  /*val optimizationPipeline : List[() => BaseIRVisitor] = List(
    () => new optimize.AliasElimination {},
    () => new optimize.IdentityCastElimination {},
  )*/

  class Compiled(val ir: Module) extends CompiledModule:
    override def compilerOptions: CompilerOptions =
      val opt = CompilerOptions.default
      opt.irLogging.logModule = false
      opt
    override def name: Name = ir.name
    override def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation


  def execute(prog: Program): Map[String, Rel] =
    val genIR = GenerateIR()
    val (mods, links) = genIR.compileProgram(prog, "SouffleProgram")

    println("The program:")
    println(prog)
    println()
    println("The Links:")
    println(links)
    println()
    println("The Modules:")
    mods.foreach(println)

    /*println(prog)
    println()
    println()
    println()
    println(mod)*/

    // Lower all components individually
    val compiledProg = new CompiledProgram:
      override def linkSet: Seq[Link] = links
      override def compiledModules: Seq[CompiledModule] = mods.map { m =>
        val compiled = Compiled(m)
        //compiled.setPipeline(pipeline)
        compiled
      }

    // Create the final linked program
    val linkedModule = compiledProg.linkedModule

    println()
    println("Linked:")
    println(compiledProg.linkedModule)

    val compiled = Compiled(linkedModule)
    compiled.setPipeline(pipeline)

    println()
    println("Lowered: ")
    println(compiled.lowered)

    val engine = new Executor().instantiate(compiled)
    val rels = engine.readAll()
    rels.map { rel =>
      rel.name -> rel
    }.toMap


  test("no component test") {
    val nodeTy = Type.Name(QualifiedName(Seq("Node")))
    val prog = Program(Seq(
      ProgramContent.TypeDecl("Node", TypeDeclConstraint.EqType(Type.Symbol)),
      ProgramContent.RelationDecl(
        Seq("edge"),
        Seq(
          Attribute("n", nodeTy), Attribute("m", nodeTy)
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
          Attribute("n", nodeTy), Attribute("m", nodeTy)
        ),
        Seq(),
        None
      ),
      ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("path"))), Map()),
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
    val path = execute(prog)("path")
    val expected = Set(
      ("a", "b"), ("a", "c"), ("a", "d"),
      ("b", "b"), ("b", "c"), ("b", "d"),
      ("c", "b"), ("c", "c"), ("c", "d")
    )
    assertResult(expected)(path.toSet)
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
      ProgramContent.ComponentInit("comp2", ComponentType("Component", Seq())),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("a"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("b"), Term.StringLit("c"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("b"))),
      ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("d"))),
      ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("comp", "path"))), Map())
    ))

    val path = execute(prog)("comp$path")
    val expected = Set(
      ("a", "b"), ("a", "c"), ("a", "d"),
      ("b", "b"), ("b", "c"), ("b", "d"),
      ("c", "b"), ("c", "c"), ("c", "d")
    )
    assertResult(expected)(path.toSet)
  }

  test("nested component test - custom types") {
    val prog = Program(
      Seq(
        ProgramContent.RelationDecl(Seq("zero"), Seq(
          Attribute("x", Type.Number),
        ), Seq(), None),
        ProgramContent.ComponentDecl(ComponentType("Component", Seq()), Seq(), Seq(
          ProgramContent.TypeDecl("Base", TypeDeclConstraint.EqType(Type.Symbol)),
          ProgramContent.RelationDecl(Seq("edge"), Seq(
            Attribute("x", Type.Name(QualifiedName(Seq("Base")))),
            Attribute("y", Type.Name(QualifiedName(Seq("Base"))))
          ), Seq(), None),
          ProgramContent.ComponentDecl(ComponentType("InnerComponent", Seq()), Seq(), Seq(
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
            ),
            ProgramContent.Fact(QualifiedName(Seq("zero")), Seq(Term.NumberLit(5)))
          )),
          ProgramContent.ComponentInit("innerComp", ComponentType("InnerComponent", Seq()))
        )),
        ProgramContent.ComponentInit("comp", ComponentType("Component", Seq())),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("a"), Term.StringLit("b"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("b"), Term.StringLit("c"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("b"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("d"))),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("comp", "innerComp", "path"))), Map()),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("zero"))), Map())
      )
    )
    // print(prog)
    val nameRes = new NameResolution {}
    nameRes.resolveProgram(prog)
    println(prog)

    val path = execute(prog)("comp$innerComp$path")
    val expected = Set(
      ("a", "b"), ("a", "c"), ("a", "d"),
      ("b", "b"), ("b", "c"), ("b", "d"),
      ("c", "b"), ("c", "c"), ("c", "d")
    )
    assertResult(expected)(path.toSet)
  }

  test("nested component test - nested types") {
    val prog = Program(
      Seq(
        ProgramContent.ComponentDecl(ComponentType("Component", Seq()), Seq(), Seq(
          ProgramContent.ComponentDecl(ComponentType("InnerComponent", Seq()), Seq(), Seq(
            ProgramContent.TypeDecl("Nat", ADTType(Seq(
              ADTConstructor("Zero", Seq()),
              ADTConstructor("Succ", Seq(Attribute("pred", Type.Name(QualifiedName(Seq("Nat")))))),
            ))),
          )),
          ProgramContent.ComponentInit("innerComp", ComponentType("InnerComponent", Seq()))
        )),
        ProgramContent.ComponentInit("comp", ComponentType("Component", Seq())),
        ProgramContent.RelationDecl(
          Seq("nats"),
          Seq(Attribute("n", Type.Name(QualifiedName(Seq("comp", "innerComp", "Nat"))))),
          Seq(), None
        ),
        ProgramContent.Rule(
          Seq(Atom.Call(QualifiedName(Seq("nats")), Seq(Term.Var("n")))),
          Seq(Atom.Compare(Term.Var("n"), Comparator.EQ, Term.Constr(QualifiedName(Seq("comp", "innerComp", "Succ")), Seq(Term.Constr(QualifiedName(Seq("comp", "innerComp", "Zero")), Seq()))))),
          None
        ),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("nats"))), Map()),
      )
    )
    val nameRes = new NameResolution {}
    nameRes.resolveProgram(prog)

    val nats = execute(prog)("nats")
    assertResult("comp$innerComp$Succ(comp$innerComp$Zero())")(nats.entries.head.toString)
  }

  test("nested component test - no custom types") {
    val prog = Program(
      Seq(
        ProgramContent.RelationDecl(Seq("zero"), Seq(
          Attribute("x", Type.Number),
        ), Seq(), None),
        ProgramContent.ComponentDecl(ComponentType("Component", Seq()), Seq(), Seq(
          ProgramContent.RelationDecl(Seq("edge"), Seq(
            Attribute("x", Type.Symbol),
            Attribute("y", Type.Symbol)
          ), Seq(), None),
          ProgramContent.ComponentDecl(ComponentType("InnerComponent", Seq()), Seq(), Seq(
            ProgramContent.RelationDecl(Seq("path"), Seq(
              Attribute("x", Type.Symbol),
              Attribute("y", Type.Symbol)
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
            ),
            ProgramContent.Fact(QualifiedName(Seq("zero")), Seq(Term.NumberLit(5)))
          )),
          ProgramContent.ComponentDecl(ComponentType("InnerComponent2", Seq()), Seq(), Seq(
            ProgramContent.RelationDecl(Seq("path"), Seq(
              Attribute("x", Type.Symbol),
              Attribute("y", Type.Symbol)
            ), Seq(), None),
            ProgramContent.Rule(
              Seq(Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("y")))),
              Seq(Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("x"), Term.Var("y")))),
              None
            ),
            ProgramContent.Rule(
              Seq(Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("y")))),
              Seq(
                Atom.Call(QualifiedName(Seq("path")), Seq(Term.Var("x"), Term.Var("z"))),
                Atom.Call(QualifiedName(Seq("edge")), Seq(Term.Var("z"), Term.Var("y"))),
              ),
              None
            ),
            ProgramContent.Fact(QualifiedName(Seq("zero")), Seq(Term.NumberLit(5)))
          )),
          ProgramContent.ComponentInit("innerComp", ComponentType("InnerComponent", Seq())),
          ProgramContent.ComponentInit("innerComp2", ComponentType("InnerComponent2", Seq()))
        )),
        ProgramContent.ComponentInit("comp", ComponentType("Component", Seq())),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("a"), Term.StringLit("b"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("b"), Term.StringLit("c"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("b"))),
        ProgramContent.Fact(QualifiedName(Seq("comp", "edge")), Seq(Term.StringLit("c"), Term.StringLit("d"))),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("comp", "innerComp", "path"))), Map()),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("comp", "innerComp2", "path"))), Map()),
        ProgramContent.Directive(DirectiveQualifier.Output, List(QualifiedName(Seq("zero"))), Map())
      )
     )
    // print(prog)
    val nameRes = new NameResolution {}
    nameRes.resolveProgram(prog)

    val expected = Set(
      ("a", "b"), ("a", "c"), ("a", "d"),
      ("b", "b"), ("b", "c"), ("b", "d"),
      ("c", "b"), ("c", "c"), ("c", "d")
    )

    val path = execute(prog)("comp$innerComp$path")
    assertResult(expected)(path.toSet)

    val path2 = execute(prog)("comp$innerComp2$path")
    assertResult(expected)(path2.toSet)
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
        Seq(Atom.Compare(Term.Var("n"), Comparator.EQ, Term.Constr(QualifiedName(Seq("Succ")), Seq(Term.Constr(QualifiedName(Seq("Zero")), Seq()))))),
        None
      )
    ))

    val nats = execute(prog)("nats")
    assertResult("Succ(Zero())")(nats.entries.head.toString)
  }