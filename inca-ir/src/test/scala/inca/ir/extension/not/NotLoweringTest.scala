package inca.ir.extension.not

import inca.ir.*
import inca.ir.extension.*
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


class NotLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val baseIR = new BaseIR {}
  val notIR: not.IR = IR
  val lowering = new Lowering {}

  def atom(i: Int): Atom = Call(s"A_$i", Seq())
  def term(i: Int): Term = Var(s"x_$i")
  def module(language: Language, atoms: Seq[Atom]): Module =
    val mod = Module("Test", language, Seq(
      Relation("test", Seq(), Seq(Body(atoms)))
    ))
    mod

  test("simple 1") {
    val mBlock = module(notIR.language, Seq(
      Not(Eq(term(1), term(2))),
      Not(Neq(term(3), term(4))),
      Not(Call("A1", Seq())),
      Not(NegCall("A2", Seq())),
      Not(ExtensionalCall("A3", Seq())),
      Not(NegExtensionalCall("A4", Seq())),
      Not(Not(Call("A5", Seq()))),
      Not(Not(Not(Not(Call("A6", Seq())))))
    ))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      Neq(term(1), term(2)),
      Eq(term(3), term(4)),
      NegCall("A1", Seq()),
      Call("A2", Seq()),
      NegExtensionalCall("A3", Seq()),
      ExtensionalCall("A4", Seq()),
      Call("A5", Seq()),
      Call("A6", Seq())
    ))
    assertResult(bBlock)(lowered)
  }


