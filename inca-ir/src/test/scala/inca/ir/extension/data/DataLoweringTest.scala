package inca.ir.extension.data

import inca.ir.{BaseIR, Language, Term, Atom, Call, Var, Body, Module, Relation, Eq, Param, TAny}
import inca.ir.extension.block
import inca.ir.extension.disjunction
import inca.ir.extension.primitiveScala.IR as ScalaIR
import inca.ir.typing.CompilationMessage
import inca.ir.typing.Typechecker
import inca.ir.string2name

import org.scalatest.funsuite.AnyFunSuiteLike

class DataLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait Stage1IR extends BaseIR with ScalaIR with block.IR with disjunction.IR:
    override val name: String = "ScalaBlockDisjunction"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object Stage1IR extends Stage1IR {}

  val dataIR: IR = IR
  val stage1lowering = ScalaLowering(dataIR, Stage1IR)
  //val stage2lowering = block.Lowering(Stage1IR, ScalaIR)

  val typechecker = new Typechecker {}

  def stopIfNeeded(): Unit = {
    val errors = typechecker.getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }

  def atom(i: Int): Atom = Call(s"A_$i", Seq())
  def term(i: Int): Term = Var(s"x_$i")
  def module(language: Language, data: Seq[DataDefinition], atoms: Seq[Atom]): Module =
    val mod = Module("Test", language,
      Relation("test", Seq(Param("p$0", TAny)), Seq(Body(atoms))) +: data
    )
    typechecker.typecheck(mod)
    stopIfNeeded()
    mod

  test("List DataDef") {
    val mData = module(dataIR.language,
      Seq(
        DataDefinition("List", Seq(
          CaseDefinition("Nil", Seq()),
          CaseDefinition("Cons", Seq(TAny, TData("List")))
        ))
      ),
      Seq(
        Eq(term(0), Construct("Nil", Seq()))
      )
    )
    println(mData)
    println()
    println(stage1lowering.lower(mData))
  }