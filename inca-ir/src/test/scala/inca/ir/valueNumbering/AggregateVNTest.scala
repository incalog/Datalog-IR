package inca.ir.valueNumbering

import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, RefByName, Relation, TermArg, Var, Module as IRModule}
import inca.ir.extension.arithmetic.{ArithmeticAggregationOperator, IntNum, TInt}
import inca.ir.extension.{arithmetic, string, aggregate}
import inca.ir.*


class AggregateVNTest extends ValueNumberingTestAbstract {

  test("param binding in aggregate"){ // added aggregate.typechecker in typing\Typechecker for this to work (with compiled programs there was no problem)
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new aggregate.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Aggregate(RefByName(Name("S1")), Seq(AggregateColumnArg(Var("a"))), ArithmeticAggregationOperator.SumInt),
            Eq(Var("a"), Var("b"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new aggregate.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Aggregate(RefByName(Name("S1")), Seq(AggregateColumnArg(Var("a"))), ArithmeticAggregationOperator.SumInt),
            Eq(Var("b"), Var("a"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

}
