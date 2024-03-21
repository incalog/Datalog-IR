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