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
      typecheckerBefore.checkProgram(Seq(mod))
      println(mod)
      printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      println("Lowered:\n" + lowered)
      typecheckerAfter.checkProgram(Seq(lowered))
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
        DataDefinition(Name("Pair"))),
      ParametricModuleEntry(Seq(Name("A")),
        CaseDefinition(Name("MkPair"), Seq(TypeVar(Name("A")), TypeVar(Name("A"))),
          TData(TypeApplication("Pair", Seq(TypeVar("A"))))
        )
      ),
      Relation("R", Seq(Param(Name("p"), TData(TypeApplication(Name("Pair"), Seq(arithmetic.TInt))))), Seq(
        Body(Seq(
          Eq(Var("p"),
            Construct(TypeApplication("MkPair", Seq(arithmetic.TInt)),
              Seq(arithmetic.IntNum(1), arithmetic.IntNum(2))))
        )),
        Body(Seq(
          Eq(Var("q"),
            Construct(TypeApplication("MkPair", Seq(string.TString)),
              Seq(string.StringLit("a"), string.StringLit("b")))),
          Eq(Var("p"),
            Construct(TypeApplication("MkPair", Seq(arithmetic.TInt)),
              Seq(arithmetic.IntNum(1), arithmetic.IntNum(2))))
        ))
      )),
    )
  }

  test("Mono morph recursive data type") {
    val mod = module(
      ParametricModuleEntry(Seq(Name("A")),
        DataDefinition(Name("List"))),
      ParametricModuleEntry(Seq("A"),
        CaseDefinition("Nil", Seq(), TData(TypeApplication("List", Seq(TypeVar("A")))))),
      ParametricModuleEntry(Seq("B"),
        CaseDefinition("Cons", Seq(TypeVar("B"), TData(TypeApplication("List", Seq(TypeVar("B"))))),
          TData(TypeApplication("List", Seq(TypeVar("B"))))
        )
      ),
      Relation("R", Seq(Param(Name("p"), TData(TypeApplication(Name("List"), Seq(arithmetic.TInt))))), Seq(
        Body(Seq(
          Eq(Var("p"),
            Construct(TypeApplication("Nil", Seq(arithmetic.TInt)), Seq())
          )
        )),
        Body(Seq(
          Call("R", Seq(Var("tail"))),
          Eq(Var("p"),
            Construct(TypeApplication("Cons", Seq(arithmetic.TInt)), Seq(arithmetic.IntNum(1), Var("tail")))
          )
        ))
      )),
    )
  }

  test("Mono morph recursive data type transitively through relation") {
    val mod = module(
      ParametricModuleEntry(Seq(Name("A")),
        DataDefinition(Name("List"))),
      ParametricModuleEntry(Seq("A"),
        CaseDefinition("Nil", Seq(), TData(TypeApplication("List", Seq(TypeVar("A")))))),
      ParametricModuleEntry(Seq("A"),
        CaseDefinition("Cons", Seq(TypeVar("A"), TData(TypeApplication("List", Seq(TypeVar("A"))))),
          TData(TypeApplication("List", Seq(TypeVar("A"))))
        )
      ),
      ParametricModuleEntry(Seq(Name("A")),
        Relation("nil", Seq(Param(Name("p"), TData(TypeApplication(Name("List"), Seq(TypeVar("A")))))), Seq(
          Body(Seq(
            Eq(Var("p"),
              Construct(TypeApplication("Nil", Seq(TypeVar("A"))), Seq())
            )
          ))
        ))
      ),
      Relation("list", Seq(Param(Name("q"), TData(TypeApplication(Name("List"), Seq(arithmetic.TInt))))), Seq(
        Body(Seq(
          Call(TypeApplication(Name("nil"), Seq(arithmetic.TInt)), Seq(Var("N").arg), false),
          Eq(Var("q"),
            Construct(TypeApplication("Cons", Seq(arithmetic.TInt)), Seq(arithmetic.IntNum(1), Var("N")))
          )
        ))
      ))
    )
  }


