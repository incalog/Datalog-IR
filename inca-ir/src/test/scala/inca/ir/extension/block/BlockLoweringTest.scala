package inca.ir.extension.block

import inca.ir.*
import inca.ir.extension.*
import inca.ir.typing.{CompilationMessage, Typechecker}
import org.scalatest.funsuite.AnyFunSuiteLike


class BlockLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val baseIR = new BaseIR {}
  val blockIR: block.IR = IR
  val lowering = new Lowering {}

  def atom(i: Int): Atom = Call(s"A_$i", Seq())
  def term(i: Int): Term = Var(s"x_$i")

  def module(language: Language, atoms: Seq[Atom]): Module =
    val mod = Module("Test", language, Seq(
      Relation("test", Seq(), Seq(Body(atoms)))
    ))
    mod

  test("simple 1") {
    val mBlock = module(blockIR.language, Seq(
      Eq(
        Block(Seq(atom(1), atom(2)), term(3)),
        Block(Seq(atom(4), atom(5)), term(6))
    )))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      atom(1), atom(2),
      atom(4), atom(5),
      Eq(term(3), term(6))
    ))
    assertResult(bBlock)(lowered)
  }

  test("simple 2") {
    val mBlock = module(blockIR.language, Seq(
      Eq(
        Block(Seq(atom(1), atom(2)), term(3)),
        Block(Seq(atom(4), atom(5)), term(6))
      ),
      Eq(
        Block(Seq(atom(7), atom(8)), term(9)),
        Block(Seq(atom(10), atom(11)), term(12))
      )))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      atom(1), atom(2),
      atom(4), atom(5),
      Eq(term(3), term(6)),
      atom(7), atom(8),
      atom(10), atom(11),
      Eq(term(9), term(12))
    ))
    assertResult(bBlock)(lowered)
  }

  test("empty block") {
    val mBlock = module(blockIR.language, Seq(
      Eq(
        Block(Seq(), term(3)),
        Block(Seq(), term(6))
      ),
      ))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      Eq(term(3), term(6))
    ))
    assertResult(bBlock)(lowered)
  }

  test("nested 1") {
    val mBlock = module(blockIR.language, Seq(
      Eq(
        Block(Seq(atom(1), atom(2)),
          Block(Seq(atom(3), atom(4)), term(5))
        ),
        Block(Seq(atom(6), atom(7)), term(8))
      )))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      atom(1), atom(2),
      atom(3), atom(4),
      atom(6), atom(7),
      Eq(term(5), term(8))
    ))
    assertResult(bBlock)(lowered)
  }

  test("nested 2") {
    val mBlock = module(blockIR.language, Seq(
      Eq(
        Block(Seq(
          Eq(
            Block(Seq(atom(1), atom(2)), term(3)),
            Block(Seq(atom(4)), term(5))
          ), atom(6)), term(7)),
        Block(Seq(atom(8), atom(9)), term(10))
      )))
    val lowered = lowering.lower(mBlock)
    val bBlock = module(baseIR.language, Seq(
      atom(1), atom(2), atom(4),
      Eq(term(3), term(5)),
      atom(6), atom(8), atom(9),
      Eq(term(7), term(10))
    ))
    assertResult(bBlock)(lowered)
  }

