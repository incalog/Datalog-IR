package inca.viatra.string.aggregate

import inca.ir.*
import inca.ir.execution.UnitRelation
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, IntNum, TInt, IR as arithIR}
import inca.ir.extension.string.{OrdinalNumber, RegexMatch, StringLength, StringLit, Substring, TString, IR as strIR}
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.Typechecker
import inca.ir.hints.MainHint
import inca.ir.typing.{BaseIRTypechecker, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class StringTest extends AnyFunSuite:
  private def compile(mod: Module): CompiledUnit =
    new CompiledUnit {
      def compilerOptions: CompilerOptions = CompilerOptions.default
      def name: Name = mod.name
      def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      def isClosedWorld: Boolean = true
      def irModules: Seq[Module] = Seq(mod)
      def otherUnits: Seq[CompiledUnit] = Seq()
      override def typechecker: BaseIRTypechecker = new IRTypechecker with Typechecker {}
    }

  def executeMain(compiled: CompiledUnit): inca.ir.execution.Relation =
    val engine = new inca.viatra.backend.Executor().instantiate(compiled)
    engine.read(UnitRelation("main"))

  test("Substring") {
    val mod = Module("CountAggregate", BaseIR.language + arithIR + strIR, Seq(
      Relation("main", Seq(
        Param("x", TString)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), Substring(StringLit("Hello world!"), IntNum(5), IntNum(3)))
        ))
      )).addHint(MainHint),
    ))
    val compiled = compile(mod)
    val res = executeMain(compiled)
    assertResult(Set(" wo"))(res.toSet)
  }

  test("StringLength") {
    val mod = Module("CountAggregate", BaseIR.language + arithIR + strIR, Seq(
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), StringLength(StringLit("Hello world!")))
        ))
      )).addHint(MainHint),
    ))
    val compiled = compile(mod)
    val res = executeMain(compiled)
    assertResult(Set(12))(res.toSet)
  }

  test("OrdinalNumber") {
    val s = "Hello world!"
    val mod = Module("CountAggregate", BaseIR.language + arithIR + strIR, Seq(
      Relation("main", Seq(
        Param("x", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), OrdinalNumber(StringLit(s)))
        ))
      )).addHint(MainHint),
    ))
    val compiled = compile(mod)
    val res = executeMain(compiled)
    assertResult(Set(s.hashCode))(res.toSet)
  }

  test("RegexMatch") {
    val s = "Hello world!"
    val mod = Module("CountAggregate", BaseIR.language + arithIR + strIR, Seq(
      Relation("words", Seq(
        Param("x", TString)
      ), Seq(
        Body(Seq(Eq(Var("x"), StringLit("a.test")))),
        Body(Seq(Eq(Var("x"), StringLit("b.test")))),
      )).addHint(MainHint),
      Relation("main", Seq(
        Param("w", TString),
        Param("body", TInt)
      ), Seq(
        Body(Seq(
          // Positive regex / match
          Call("words", Seq(Var("w"))),
          RegexMatch(Var("w"), StringLit("a.*"), false),
          Eq(Var("body"), IntNum(0))
        )),
        Body(Seq(
          // Negative regex / not match
          Call("words", Seq(Var("w"))),
          RegexMatch(Var("w"), StringLit("a.*"), true),
          Eq(Var("body"), IntNum(1))
        ))
      )).addHint(MainHint),
    ))
    val compiled = compile(mod)
    val res = executeMain(compiled)
    assertResult(Set(("a.test", 0), ("b.test", 1)))(res.toSet)
  }