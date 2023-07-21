package inca.ir.extension.arithmetic

import inca.ir.*
import inca.ir.extension.*
import inca.ir.extensions.PrimitiveScalaIR
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


// TODO: Add support for Visiting PrimitiveIR
// TODO: Discuss: LT, GT how do we handle the scala boolean return ?
// TODO: Discuss: for required parameter to be correct arithmetic IR needs to extend
//  PrimitiveScalaIR. block.IR is per se not required, but its nice to have to express infix operators.
//  Thats why lower produces a result with a block inside. Is this required or not ?

class ArithmeticLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait Stage1IR extends PrimitiveScalaIR with block.IR:
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
  val stage2lowering: block.Lowering[Stage1IR, PrimitiveScalaIR] = new block.Lowering[Stage1IR, PrimitiveScalaIR] {
    override def src = stage1IR
    override def trg = PrimitiveScalaIR
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
      Eq(Var("x"), Add(term(0), term(1)))
    ))
    val lowered = stage2lowering.lower(stage1lowering.lower(mAdd))
    val bAdd = module(stage1IR.language, Seq(
      atom(1), atom(2),
      atom(4), atom(5),
      Eq(term(3), term(6))
    ))
    println(lowered)
    //assertResult(bAdd)(lowered)
  }

