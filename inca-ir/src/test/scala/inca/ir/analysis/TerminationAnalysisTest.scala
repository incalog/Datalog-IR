package inca.ir.analysis

import inca.ir.analysis.base.values.{AbstractRelation, Value}
import inca.ir.extension.arithmetic.analysis.interpreter.ConstantIntV
import inca.ir.extension.arithmetic.{Add, IntNum, GT, Mul, Sub, TInt, IR as arithIR}
import inca.ir.extension.bool.analysis.interpreter.ConstantBoolV
import inca.ir.extension.bool.{AtomAsBool, BoolFalse, BoolTrue, TBoolean, IR as boolIR}
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.map.analysis.interpreter.{ConstantMapFunV, ConstantMapV}
import inca.ir.extension.map.{MapComprehension, MapContains, MapFrom, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.not.Not
import inca.ir.extension.set.analysis.interpreter.ConstantSetV
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet, IR as setIR}
import inca.ir.extension.string.analysis.interpreter.ConstantStringV
import inca.ir.extension.string.{StringLit, TString, IR as stringIR}
import inca.ir.extension.tuple.analysis.interpreter.ConstantTupleV
import inca.ir.extension.tuple.{Project, TTuple, TupleLit, IR as tupleIR}
import inca.ir.hints.MainHint
import inca.ir.typing.IRTypechecker
import inca.ir.{BaseIR, Body, Call, Cast, Eq, ExtensionalCall, ExtensionalRelation, Module, Param, RefByName, Relation, TNothing, Var, WildcardArg, string2name, term2Arg, termList2ArgList}
import org.scalatest.funsuite.AnyFunSuiteLike
import sturdy.values.Topped

class TerminationAnalysisTest extends AnyFunSuiteLike:

  def interp(mod: Module, edb: Map[String, AbstractRelation] = Map()): Map[String, AbstractRelation] =
    val typechecker = new IRTypechecker
    typechecker.checkProgram(Seq(mod))

    val abstractInterp = IRTerminationAbstractInterpreter(interRelational = true)
    edb.foreach(abstractInterp.insertEDB)
    abstractInterp.evalProgram(Seq(mod))
    abstractInterp.getIDB

  test("Factorial") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("input", Seq(
        Param("n", TInt),
      ), Seq(
        Body(Seq(
          Call("input", Seq(Var("m"))),
          GT(Var("m"), IntNum(1)),
          Eq(Var("n"), Sub(Var("m"), IntNum(1)))
        )),
        Body(Seq(
          Eq(Var("n"), IntNum(1000)),
        ))
      )).addHint(MainHint),
    ))

    val res = interp(mod)
    println(res)
  }