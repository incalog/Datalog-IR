package inca.ir.extension.typeparam

import inca.ir.*
import inca.ir.extension.*
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.*
import inca.ir.typing.{IRTypechecker, Typechecker}
import inca.util.CompilationMessage
import org.scalatest.funsuite.AnyFunSuiteLike


class TypeParamLoweringTest extends AnyFunSuiteLike:
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  def module(entries: ModuleEntry*): Module =
    val typecheckerBefore = new IRTypechecker
    val typecheckerAfter = new IRTypechecker
    val lowering = new Lowering {}

    val mod = Module("M", BaseIR.language + IR + arithmetic.IR + string.IR + tuple.IR + data.IR, entries)
    var printedMod = false
    var lowered: Module = null
    try {
      typecheckerBefore.checkModule(mod)
      println(mod)
      printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerAfter.checkModule(lowered)
      lowered
    } finally {
      if (!printedMod)
        println(mod)
      println(lowered)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsAfter = typecheckerAfter.getErrors
      if (errorsBefore.nonEmpty) {
        println("Type errors in original code:")
        errorsBefore.foreach(println)
      }
      if (errorsAfter.nonEmpty) {
        println("Type errors in lowered code:")
        errorsAfter.foreach(println)
      }
    }

  test("Mono morph direct usages") {
    val mod = module(
      ParametricModuleEntry(Seq(Name("A")), Relation("Empty", Seq(Param("a", TypeVar(Name("A")))), Seq())),
      Relation("R", Seq(), Seq(
        Body(Seq(
          Call(TypeApplication(Name("Empty"), Seq(arithmetic.TInt)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), arithmetic.IntNum(3))
        )),
        Body(Seq(
          Call(TypeApplication(Name("Empty"), Seq(string.TString)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), string.StringLit("test"))
        ))
      )),
    )
  }

  test("Mono morph indirect usages") {
    val mod = module(
      ParametricModuleEntry(Seq(Name("A")), Relation("Empty", Seq(Param("a", TypeVar(Name("A")))), Seq())),
      ParametricModuleEntry(Seq(Name("B")), Relation("Foo", Seq(Param("b", TypeVar(Name("B")))), Seq(
        Body(Seq(
          Call(TypeApplication("Empty", Seq(TypeVar("B"))), Seq(TermArg(Var("b"))), false)
        ))
      ))),
      Relation("R", Seq(), Seq(
        Body(Seq(
          Call(TypeApplication(Name("Empty"), Seq(tuple.TTuple(Seq(arithmetic.TInt, arithmetic.TInt)))), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), tuple.TupleLit(Seq(arithmetic.IntNum(3), arithmetic.IntNum(3))))
        )),
        Body(Seq(
          Call(TypeApplication(Name("Foo"), Seq(arithmetic.TInt)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), arithmetic.IntNum(3))
        )),
        Body(Seq(
          Call(TypeApplication(Name("Foo"), Seq(string.TString)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), string.StringLit("test"))
        ))
      )),
    )
  }

  test("Mono morph non-recursive data type") {
    val mod = module(
      ParametricModuleEntry(Seq(Name("A")),
        DataDefinition(Name("Pair"), Seq(
          CaseDefinition(Name("MkPair"), Seq(TypeVar(Name("A")), TypeVar(Name("A"))))
        ))
      ),
      Relation("R", Seq(Param(Name("p"), TData(Name("Pair")))), Seq(
        Body(Seq(
          Call(TypeApplication(Name("Empty"), Seq(arithmetic.TInt)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), arithmetic.IntNum(3))
        )),
        Body(Seq(
          Call(TypeApplication(Name("Empty"), Seq(string.TString)), Seq(TermArg(Var("a"))), false),
          Eq(Var("a"), string.StringLit("test"))
        ))
      )),
    )
  }


