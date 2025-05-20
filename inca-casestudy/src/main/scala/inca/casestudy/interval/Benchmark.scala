package inca.casestudy.interval

import inca.casestudy.interval
import inca.casestudy.interval.IntervalAnalysisMono.TAssign
import inca.casestudy.interval.edb.{Assign, Num, Sequence, While, GT, Add, Var, Skip}
import inca.ir.*
import inca.ir.execution.{Relation1, Relation2, Relation3, Relation4, UnitRelation}
import inca.ir.extension.*
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.*
import inca.ir.extension.data.*
import inca.ir.extension.demand.*
import inca.ir.extension.mono.ArithmeticMonoDefinition.SumInt
import inca.ir.extension.mono.{MapMonoDefinition, MonoImpurityKind, MonoTypes, NewMono, ReadMono, TMono, WriteMono}
import inca.ir.extension.string.*
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.typing.{DependencyGraph, IRTypechecker}
import inca.ir.util.SourceLocation
import inca.util.compileroptions.CompilerOptions
import inca.viatra.runtime.EnginePool

import scala.language.implicitConversions
import inca.foreign.scala.ir.primitive.{ConversionElimination, ForeignScalaLowering, ScalaAggregationOperator, ScalaMonoDefinition, ScalaType, Typechecker}
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.edbdata.{EdbDataModuleEntry, EdbDeconstruct, EdbFieldDefinition, EdbNodeDefinition, LookupEdbField, LookupEdbType, TEdbNode, TEdbValue}
import inca.ir.extension.map.{MapComprehension, MapLookUp, TMap, IR as mapIR}
import inca.viatra.runtime.context.DataModel
import inca.ir.extension.data.IR as dataIR
import inca.foreign.scala.ir.mono.MonoLowering as MonoScalaLowering
import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.{arithmetic as scalaArith, data as scalaData, string as scalaString}
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.edbdata.Link.Parent
import inca.ir.extension.impure.Impure
import inca.viatra.backend.Executor
import inca.viatra.runtime.EnginePool
import inca.viatra.runtime.context.DataModel
import org.eclipse.viatra.query.runtime.rete.matcher.DRedReteBackendFactory

import scala.language.implicitConversions

object Benchmark:
  val example1 = {
    val a1 = Assign(
      "x", Num(4)
    )
    val a2 = Assign(
      "y", Add(Num(5), Var("x"))
    )
    val a3 = Assign(
      "z", Num(2)
    )
    Sequence(Sequence(a1, a2), a3)
  }

  val example2 = {
    val a1 = Assign(
      "x", Num(1)
    )

    val a2 = edb.While(
      GT(interval.edb.Var("x"), interval.edb.Num(0)),
      edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1)))
    )
    Sequence(a1, a2)
  }

  val example3 = {
    Sequence(
      Assign("z", Num(1)),
      While(GT(Var("x"), Num(0)),
        Sequence(
          Assign("z", Add(edb.Var("z"), Var("y"))),
          Assign("x", Add(Var("x"), Num(-1))))))
  }

  val example4 = {
    Sequence(
      Assign("x", Add(Var("a"), Var("b"))),
      Sequence(
        Assign("y", Add(Var("a"), Var("b"))),
        While(GT(Var("y"), Add(Var("a"), Var("b"))),
          Sequence(
            Assign("a", Add(Var("a"), Num(1))),
            Assign("x", Add(Var("a"), Var("b")))))))
  }

  val example5 = {
    Sequence(
      Assign("x", Num(5)),
      Sequence(
        Assign("y", Num(1)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Assign("x", Add(Var("x"), Num(-1)))))))
  }

  val example6 = {
    Sequence(
      Assign("x", Num(1)),
      While(GT(Var("x"), Num(1)),
        Assign("x", Add(Var("x"), Num(-1)))))
  }

  /*
     x = 2
     y = 2
     while(x > 1) {
       y = x + y
       x = x + 2
     }
    */
  val example7 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))


  // lhs of assign in while loop adds + 1
  // y = x + y -> y = (x + y) + 1
  val example8 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Add(Var("x"), Var("y")), Num(1))),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))

  // insert y = y after y = x + y
  val example9 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Sequence(
              Assign("y", Add(Var("x"), Var("y"))),
              Assign("y", Var("y"))
            ),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))


  // change initial assignment of x to 3 instead of 2

  val example10 =
    Sequence(
      Assign("x", Num(3)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))


  // sub millisecond update time
  // introduce new var before while
  val example11 =
    Sequence(
      Sequence(
        Assign("z", Num(1)),
        Assign("x", Num(2)),
      ),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Skip(),
              Assign("x", Add(Var("x"), Num(2))))))))


  // update times around 5-8 ms
  // introduce var that is static in loop
  val example12 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Assign("z", Num(12)),
              Assign("x", Add(Var("x"), Num(2))))))))


  // update times around 10-13 ms
  // introduce var in loop that changes it value each iteration
  val example13 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Assign("y", Num(2)),
        While(GT(Var("x"), Num(1)),
          Sequence(
            Assign("y", Add(Var("x"), Var("y"))),
            Sequence(
              Assign("z", Add(Var("y"), Num(3))),
              Assign("x", Add(Var("x"), Num(2))))))))

  // add assign after while
  // x = x + 12
  // takes > 200 ms
  val example14 =
    Sequence(
      Assign("x", Num(2)),
      Sequence(
        Sequence(
          Assign("y", Num(2)),
          While(GT(Var("x"), Num(1)),
            Sequence(
              Assign("y", Add(Var("x"), Var("y"))),
              Sequence(
                Skip(),
                Assign("x", Add(Var("x"), Num(2))))))),
        Assign("x", Add(Var("x"), Num(12)))))


  /*
    x = 10
    y = 1
    while (10 > x) {
      x = x + 1
      while (x > y) {
        y = x + y
        while (x > 10) {
          y = y + y
          while (y > 10) {
            x = x + 1
          }
        }
      }
    }
   */
  val example15 =

    Sequence(
      Assign("x", Num(10)),
      Sequence(
        Assign("y", Num(1)),
        While(
          GT(Num(10), Var("x")),
          Sequence(
            Assign("x", Add(Var("x"), Num(1))),
            While(
              GT(Var("x"), Var("y")),
              Sequence(
                Assign("y", Add(Var("x"), Var("y"))),
                While(
                  GT(Var("x"), Num(10)),
                  Sequence(
                    Assign("y", Add(Var("y"), Var("y"))),
                    While(
                      GT(Var("y"), Num(10)),
                      Assign("x", Add(Var("x"), Num(1)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )


  /*
    x = 0
    while (x < 1000) {
      x = x + 1
      y = 0
      while (y < 1000) {
        y = y + 1
      }
    }
   */
  val example16 =

    Sequence(
      Assign("x", Num(0)),
      While(
        GT(Num(1000), Var("x")),
        Sequence(
          Assign("x", Add(Var("x"), Num(1))),
          Sequence(
            Assign("y", Num(0)),
            While(
              GT(Num(1000), Var("y")),
              Assign("y", Add(Var("y"), Num(1))),
            )
          )
        )
      )
    )


  /*
   x = 0
   while (x < 1000) {
     x = x + 1
     y = 0
     while (y < 1000) {
       y = y + 1
       z = 0
       while (z < 1000) {
         z = z + 1
       }
     }
   }
   */
  val example17 =

    Sequence(
      Assign("x", Num(0)),
      While(
        GT(Num(1000), Var("x")),
        Sequence(
          Assign("x", Add(Var("x"), Num(1))),
          Sequence(
            Assign("y", Num(0)),
            While(
              GT(Num(1000), Var("y")),
              Sequence(
                Assign("y", Add(Var("y"), Num(1))),
                Sequence(
                  Assign("z", Num(0)),
                  While(
                    GT(Num(1000), Var("z")),
                    Assign("z", Add(Var("z"), Num(1))),
                  )
                )
              )
            )
          )
        )
      )
    )


  val example18 =

    Sequence(
      Assign("x", Num(99)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )

  val example19 =

    Sequence(
      Assign("x", Num(100)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )

  val example20 =

    Sequence(
      Assign("x", Num(98)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )


  val example21 =

    Sequence(
      Assign("x", Num(1)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )

  val example22 =

    Sequence(
      Assign("x", Num(2)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )

  val example23 =

    Sequence(
      Assign("x", Num(0)),
      While(
        GT(Var("x"), Num(0)),
        Assign("x", Add(Var("x"), Num(1)))
      )
    )

  val example24 =

    Sequence(
      Assign("x", Num(1)),
      While(
        GT(Var("x"), Num(0)),
        Sequence(
          Assign("x", Add(Var("x"), Num(1))),
          Skip()
        )
      )
    )

  val example25 =

    Sequence(
      Assign("x", Num(1)),
      While(
        GT(Var("x"), Num(0)),
        Sequence(
          Skip(),
          Assign("x", Add(Var("x"), Num(1)))
        )
      )
    )

  // Example used in our Paper: "Mono Types — First-Class Containers for Datalog" section 5.2
  def nestedWhileProgram(nestings: Int, repetitions: Int): edb.Stmt = {
    def nestedWhile(levels: Int): edb.Stmt =
      if (levels == 0)
        edb.Sequence(
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(-1))),
          edb.Assign("x", edb.Add(edb.Var("x"), edb.Num(1)))
        )
      else
        edb.While(
          edb.GT(edb.Var("x"), edb.Num(0)),
          nestedWhile(levels - 1)
        )

    def sequence(s: () => edb.Stmt, counts: Int): edb.Stmt =
      if (counts == 0)
        s()
      else
        edb.Sequence(s(), sequence(s, counts - 1))

    edb.Sequence(
      edb.Assign("x", edb.Num(1)),
      edb.Sequence(
        sequence(() => nestedWhile(nestings), repetitions),
        edb.Exit()))
  }

