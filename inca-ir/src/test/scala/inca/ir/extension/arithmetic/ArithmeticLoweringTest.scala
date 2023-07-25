package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.primitiveScala.{Application, Constant, IR as ScalaIR}
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


// TODO: Add support for Visiting PrimitiveIR
// TODO: Discuss: LT, GT how do we handle the scala boolean return ?
// TODO: Discuss: for required parameter to be correct arithmetic IR needs to extend
//  PrimitiveScalaIR. block.IR is per se not required, but its nice to have to express infix operators.
//  Thats why lower produces a result with a block inside. Is this required or not ?

class ArithmeticLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait Stage1IR extends ScalaIR with block.IR:
    override val name: String = "TrgIR"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object Stage1IR extends Stage1IR {}

  val stage1IR = Stage1IR
  val arithmeticIR: arithmetic.IR = IR
  val stage1lowering: Lowering[IR, Stage1IR] = new Lowering[IR, Stage1IR] {
    override def src = arithmeticIR
    override def trg = stage1IR
  }
  val stage2lowering: block.Lowering[Stage1IR, ScalaIR] = new block.Lowering[Stage1IR, ScalaIR] {
    override def src = stage1IR
    override def trg = ScalaIR
  }
  val typecker = new Typechecker {}

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
    typecker.typecheck(mod)
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

