package inca.ir.extension.not

import inca.ir.*
import inca.ir.extension.*
import inca.ir.typing.Typechecker
import inca.util.CompilationMessage
import org.scalatest.funsuite.AnyFunSuiteLike


class NotLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val baseIR: BaseIR = new BaseIR {}
  val notIR: not.IR = IR
  val lowering: Lowering = new Lowering {}

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
      Not(Eq(term(3), term(4), true)),
      Not(Call("A1", Seq())),
      Not(Call("A2", Seq(), true)),
      Not(ExtensionalCall("A3", Seq())),
      Not(ExtensionalCall("A4", Seq(), true)),
      Not(Not(Call("A5", Seq()))),
      Not(Not(Not(Not(Call("A6", Seq())))))
    ))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      Eq(term(1), term(2), true),
      Eq(term(3), term(4)),
      Call("A1", Seq(), true),
      Call("A2", Seq()),
      ExtensionalCall("A3", Seq(), true),
      ExtensionalCall("A4", Seq()),
      Call("A5", Seq()),
      Call("A6", Seq())
    ))
    assertResult(bBlock)(lowered)
  }


