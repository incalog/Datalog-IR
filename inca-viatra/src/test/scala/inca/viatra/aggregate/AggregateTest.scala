package inca.viatra.aggregate

import inca.ir.*
import inca.ir.execution.UnitRelation
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, IntNum, TInt, IR as arithIR}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, IR as aggIR}
import inca.ir.hints.MainHint
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import org.scalatest.funsuite.AnyFunSuite

class AggregateTest extends AnyFunSuite:
  private def compile(mod: Module): CompiledUnit =
    new CompiledUnit {
      def compilerOptions: CompilerOptions = CompilerOptions.default
      def name: Name = mod.name
      def sourceLocation: SourceLocation = SourceLocation.NoSourceLocation
      def isClosedWorld: Boolean = true
      def irModules: Seq[Module] = Seq(mod)
      def otherUnits: Seq[CompiledUnit] = Seq()
    }

  def executeMain(compiled: CompiledUnit): inca.ir.execution.Relation =
    val engine = new inca.viatra.backend.Executor().instantiate(compiled)
    engine.read(UnitRelation("main"))

  // This test is here to show that the count aggregation always counts all values of a column, not unique values.
  // In other words, you will always get the number rows in the relation. The AggregateColumnArg does not really matter.
  test("Count aggregate") {
    val mod = Module("CountAggregate", BaseIR.language + arithIR + aggIR, Seq(
      Relation("test", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("main", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Aggregate("test", Seq(AggregateColumnArg(Var("x")), WildcardArg()), ArithmeticAggregationOperator.Count),
          Aggregate("test", Seq(AggregateColumnArg(Var("y")), WildcardArg()), ArithmeticAggregationOperator.Count),
        ))
      )).addHint(MainHint),
    ))
    val compiled = compile(mod)
    val res = executeMain(compiled)
    assertResult(Set((2, 2)))(res.toSet)
    println(res.asTable)
  }