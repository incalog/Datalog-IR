package inca.ir.analysis

import inca.ir.{BaseIR, Body, Call, CompiledUnit, Eq, Language, Module, Name, Param, Relation, Var, string2name, term2Arg, termList2ArgList}
import inca.ir.extension.arithmetic.{Add, IntNum, Mul, TInt, IR as arithIR}
import inca.ir.extension.impure.MainHint
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuiteLike

case class CompiledTestUnit(mod: Module) extends CompiledUnit:
  def compilerOptions: CompilerOptions = CompilerOptions.default
  def name: Name = mod.name
  def sourceLocation: SourceLocation = Name("Test")

  def isClosedWorld: Boolean = true
  def irModules: Seq[Module] = Seq(mod)
  def otherUnits: Seq[CompiledUnit] = Seq()


class SimpleTest extends AnyFunSuiteLike:

  test("Single relation - arithmetic") {
    val mod = Module("Test1", BaseIR.language + arithIR, Seq(
      Relation("main", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Add(IntNum(3), IntNum(4)), IntNum(2)) )
        ))
      )).addHint(MainHint)
    ))

    val compiled = CompiledTestUnit(mod)
    println(compiled.lowered)
  }

  test("Two relation - arithmetic") {
    val mod = Module("Test2", BaseIR.language + arithIR, Seq(
      Relation("calc", Seq(
        Param("out", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("out"), Mul(Add(IntNum(3), IntNum(4)), IntNum(2)))
        ))
      )),
      Relation("main", Seq(
        Param("res", TInt)
      ), Seq(
        Body(Seq(
          Call("calc", Seq(Var("res")))
        ))
      )).addHint(MainHint),
    ))

    val compiled = CompiledTestUnit(mod)
    println(compiled.lowered)
  }

  test("Recursive Relation") {
    val mod = Module("Test3", BaseIR.language + arithIR, Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )).addHint(MainHint),
    ))

    val compiled = CompiledTestUnit(mod)
    println(compiled.lowered)
  }
