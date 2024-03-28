package inca.ir.typing

import inca.ir.*
import inca.ir.extension.data.*
import inca.ir.extension.data.Typechecker
import org.scalatest.funsuite.AnyFunSuiteLike
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TInt}

class DataTypeCheckerTest extends AnyFunSuiteLike:

  def module(data: ModuleEntry*)(using typechecker: () => Typechecker): Module =
    val mod = Module("M", inca.ir.extension.data.IR.language, data)
    val checker = typechecker()
    try checker.checkProgram(Seq(mod))
    finally {
      println(mod)
      checker.getErrors.foreach(println)
    }
    mod

  test("very simple dataexport test") {
    implicit val typechecker = () => new Typechecker {}
    assertThrows[IllegalArgumentException] {
      module(DataDefinition("testdata"),
      DataDefinitionExport("nottestdata"))
    }
  }

  test("simple caseexport test") {
    implicit val typechecker = () => new Typechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(DataDefinition("testdata"),
      CaseDefinition("addition", Seq(TInt, TInt), TData("testdata")),
      CaseDefinitionExport("addition", Seq(TNothing, TInt), TData("testdata")))
    }
  }

  test("caseimport deconstruct test") {
    implicit val typechecker = () => new Typechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(
        DataDefinitionImport("testdata"),
        CaseDefinitionImport("addition", Seq(TInt, TInt), TData("testdata")),
        RelationImport("Q", Seq(TNothing, TNothing)),
        Relation("R", Seq(Param("test", TData("testdata"))), Seq(Body(Seq(
          Deconstruct(Var("test"), "addition", Seq(Var("x"), Var("y"))),
          Call("Q", Seq(Var("x"), Var("y")))
        ))))
      )
    }
  }

  test("caseimport construct test") {
    implicit val typechecker = () => new Typechecker with arithmetic.Typechecker {}
    assertThrows[TypeErrorException] {
      module(
        DataDefinitionImport("testdata"),
        CaseDefinitionImport("addition", Seq(TInt, TInt), TData("testdata")),
        Relation("R", Seq(Param("x", TInt), Param("y", TNothing)), Seq(Body(Seq(
          Eq(Construct("addition", Seq(Var("x"), Var("x"))), Construct("addition", Seq(Var("y"), Var("y"))))
        ))))
      )
    }
  }