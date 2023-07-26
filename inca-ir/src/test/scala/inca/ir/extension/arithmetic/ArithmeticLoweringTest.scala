package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.primitiveScala.{Application, Constant, IR as ScalaIR}
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


// TODO: Discuss: LT, GT how do we handle the scala boolean return ?
// TODO: block.IR is per se not required, but its nice to have to express infix operators.
//  Thats why lower produces a result with a block inside. This is not nice, since we need to
//  create a new intermediate IR for this.

class ArithmeticLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait Stage1IR extends BaseIR with ScalaIR with block.IR:
    override val name: String = "ScalaBlock"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object Stage1IR extends Stage1IR {}

  val arithmeticIR: arithmetic.IR = IR
  val stage1lowering = ScalaLowering(arithmeticIR, Stage1IR)
  val stage2lowering = block.Lowering(Stage1IR, ScalaIR)

  val typechecker = new Typechecker {}

  def stopIfNeeded(): Unit = {
    val errors = typechecker.getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }

  def atom(i: Int): Atom = Call(s"A_$i", Seq())
  def term(i: Int): Term = Var(s"x_$i")
  def module(language: Language, atoms: Seq[Atom]): Module =
    val mod = Module("Test", language, Seq(
      Relation("test", Seq(), Seq(Body(atoms)))
    ))
    typechecker.typecheck(mod)
    stopIfNeeded()
    mod

  test("simple 1") {
    val mAdd = module(arithmeticIR.language, Seq(
      Eq(term(0), IntNum(4)),
      Eq(term(1), IntNum(2)),
      Eq(term(2), Add(term(0), term(1)))
    ))
    val lowered = stage2lowering.lower(stage1lowering.lower(mAdd))

    val intTy = Scala.TypeName("Int")
    val lam = Scala.Lam(
      Seq("lhs" -> Some(intTy), "rhs" -> Some(intTy)),
      Scala.AppInfix(Scala.Id("lhs"), "+", Scala.Id("rhs"))
    )
    val tmpVar = Var("Arithmetic$0")

    val bAdd = module(ScalaIR.language, Seq(
      Eq(term(0), Constant(Scala.IntLiteral(4))),
      Eq(term(1), Constant(Scala.IntLiteral(2))),
      Application(tmpVar, lam, Seq(term(0), term(1))),
      Eq(term(2), tmpVar)
    ))
    /*println(mAdd)
    println()
    println(lowered)
    println()
    println(bAdd)*/
    assertResult(bAdd)(lowered)
  }

