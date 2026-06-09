package inca.ir.analysis

import inca.ir.analysis.base.values.{ProvenanceAbstractRelation, ProvenanceV}
import inca.ir.extension.arithmetic.{Add, DoubleNum, IntNum, TDouble, TInt, IR as arithIR}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, TData, IR as dataIR}
import inca.ir.extension.string.{StringConcat, StringLit, TString, IR as stringIR}
import inca.ir.hints.MainHint
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Eq, ExtensionalCall, ExtensionalRelation, Module, Param, Relation, TAny, Var, string2name}
import org.scalatest.funsuite.AnyFunSuiteLike

class ProvenanceAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module): Map[String, ProvenanceAbstractRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRProvenanceAbstractInterpreter(interRelational = true)
    val edbConfig = ProvenanceEdbConfig.default
    mod.entries.foreach {
      case (_, ExtensionalRelation(n, params)) =>
        abstractInterp.insertEDB(n.name, edbConfig.abstractExtensionalRelation(n, params))
      case _ => // nothing
    }
    abstractInterp.evalProgram(Seq(mod))
    abstractInterp.getIDB

  test("EDB columns seed provenance") {
    val mod = Module("Test", BaseIR.language, Seq(
      ExtensionalRelation("edge", Seq(
        Param("x", TAny),
        Param("y", TAny),
      )),
      Relation("path", Seq(
        Param("x", TAny),
        Param("y", TAny),
      ), Seq(
        Body(Seq(
          ExtensionalCall("edge", Seq(Var("x").arg, Var("y").arg)),
        ))
      )).addHint(MainHint),
    ))

    val path = interp(mod)("path")

    assert(path.cols == Seq("x", "y"))
    assert(path.rows == Seq(ProvenanceV("edb.edge.x"), ProvenanceV("edb.edge.y")))
  }

  test("joins merge shared-column provenance") {
    val mod = Module("Test", BaseIR.language, Seq(
      ExtensionalRelation("edge", Seq(
        Param("x", TAny),
        Param("y", TAny),
      )),
      Relation("twoHop", Seq(
        Param("x", TAny),
        Param("y", TAny),
        Param("z", TAny),
      ), Seq(
        Body(Seq(
          ExtensionalCall("edge", Seq(Var("x").arg, Var("y").arg)),
          ExtensionalCall("edge", Seq(Var("y").arg, Var("z").arg)),
        ))
      )).addHint(MainHint),
    ))

    val twoHop = interp(mod)("twoHop")

    assert(twoHop.cols == Seq("x", "y", "z"))
    assert(twoHop.rows == Seq(
      ProvenanceV("edb.edge.x"),
      ProvenanceV(Set("edb.edge.x", "edb.edge.y")),
      ProvenanceV("edb.edge.y"),
    ))
  }

  test("definitely failing filter keeps provenance relation") {
    val mod = Module("Test", BaseIR.language, Seq(
      ExtensionalRelation("edge", Seq(
        Param("x", TAny),
        Param("y", TAny),
      )),
      Relation("fail", Seq(
        Param("x", TAny),
      ), Seq(
        Body(Seq(
          ExtensionalCall("edge", Seq(Var("x").arg, Var("y").arg)),
          Eq(Var("x"), Var("x"), neg = true),
        ))
      )).addHint(MainHint),
    ))

    val fail = interp(mod)("fail")

    assert(fail.cols == Seq("x"))
    assert(fail.rows == Seq(ProvenanceV("edb.edge.x")))
  }

  test("runtime int terms use runtime.int provenance") {
    val mod = Module("Test", BaseIR.language + arithIR, Seq(
      Relation("calc", Seq(
        Param("out", TInt),
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Add(IntNum(1), IntNum(2))),
        ))
      )).addHint(MainHint),
    ))

    val calc = interp(mod)("calc")

    assert(calc.cols == Seq("out"))
    assert(calc.rows == Seq(ProvenanceV("runtime.int")))
  }

  test("runtime double terms use runtime.double provenance") {
    val mod = Module("Test", BaseIR.language + arithIR, Seq(
      Relation("calc", Seq(
        Param("out", TDouble),
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Add(DoubleNum(1.0), DoubleNum(2.0))),
        ))
      )).addHint(MainHint),
    ))

    val calc = interp(mod)("calc")

    assert(calc.cols == Seq("out"))
    assert(calc.rows == Seq(ProvenanceV("runtime.double")))
  }

  test("runtime string terms use runtime.string provenance") {
    val mod = Module("Test", BaseIR.language + stringIR, Seq(
      Relation("concat", Seq(
        Param("out", TString),
      ), Seq(
        Body(Seq(
          Eq(Var("out"), StringConcat(StringLit("a"), StringLit("b"))),
        ))
      )).addHint(MainHint),
    ))

    val concat = interp(mod)("concat")

    assert(concat.cols == Seq("out"))
    assert(concat.rows == Seq(ProvenanceV("runtime.string")))
  }

  test("runtime data terms use runtime.data provenance") {
    val listTy = TData("List")
    val mod = Module("Test", BaseIR.language + dataIR, Seq(
      DataDefinition("List"),
      CaseDefinition("Nil", Seq(), listTy),
      Relation("nil", Seq(
        Param("out", listTy),
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Construct("Nil", Seq())),
        ))
      )).addHint(MainHint),
    ))

    val nil = interp(mod)("nil")

    assert(nil.cols == Seq("out"))
    assert(nil.rows == Seq(ProvenanceV("runtime.data")))
  }
