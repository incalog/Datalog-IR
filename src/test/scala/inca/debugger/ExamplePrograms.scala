package inca.debugger

import inca.backend.ir.Datalog
import inca.debugger.table.ImmutableTable
import inca.util.Scala
import scala.meta.XtensionQuasiquoteTerm

object ExamplePrograms {
  def module(pats: Datalog.Pattern*): Datalog.Module =
    Datalog.Module("TestModule", Seq(), pats, Seq())

  def edgePattern(edges: (Int, Int)*): Datalog.Pattern =
    Datalog.Pattern(
      None,
      "edge",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      edges.map { case (from, to) =>
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => $from"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => $to"))
            )
          )
        )
      }
    )
  def edgeTable(columns: Seq[String], edges: (Int, Int)*): ImmutableTable[Value] = {
    val rows = edges.map { case (x, y) =>
      Seq(ScalaValue(x), ScalaValue(y))
    }
    ImmutableTable[Value](columns, rows)
  }

  val singleEdgePattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "edge",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(
                Seq((Datalog.Var("from"), Datalog.base.TScalaInt)),
                Datalog.base.TScalaInt,
                Scala(q"(x: Int) => x + 1")
              )
            )
          )
        )
      )
    )
  val twoEdgePattern: Datalog.Pattern = edgePattern(1 -> 2, 1 -> 3)
  val sevenEdgePattern: Datalog.Pattern =
    edgePattern(1 -> 2, 1 -> 4, 1 -> 5, 2 -> 3, 2 -> 6, 4 -> 6, 6 -> 7)
  val simpleCycleEdgePattern: Datalog.Pattern = edgePattern(1 -> 2, 2 -> 1)
  val simpleCycleEdgePattern2: Datalog.Pattern = edgePattern(1 -> 2, 2 -> 1, 2 -> 3)
  val cycleEdgePattern: Datalog.Pattern =
    edgePattern(1 -> 2, 1 -> 4, 1 -> 5, 2 -> 3, 2 -> 6, 4 -> 6, 6 -> 7, 3 -> 1)
  val threeHopCyclePattern: Datalog.Pattern = edgePattern(1 -> 2, 2 -> 3, 3 -> 1)

  val twoHopsModule: Datalog.Module = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      sevenEdgePattern,
      Datalog.Pattern(
        None,
        "one",
        Seq(
          Datalog.Param("from", Datalog.base.TScalaInt),
          Datalog.Param("to", Datalog.base.TScalaInt)),
        Seq(
          Datalog.Body(Seq(Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))))
        )
      ),
      Datalog.Pattern(
        None,
        "two",
        Seq(
          Datalog.Param("from", Datalog.base.TScalaInt),
          Datalog.Param("to", Datalog.base.TScalaInt)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
              Datalog.Call("one", Seq(Datalog.Var("temp"), Datalog.Var("to")))
            )
          )
        )
      )
    ),
    Seq()
  )
  val nodePattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "node",
      Seq(Datalog.Param("n", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(Seq(Datalog.Call("edge", Seq(Datalog.Var("n"), Datalog.Var("to"))))),
        Datalog.Body(Seq(Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("n")))))
      )
    )

  val negationModule: Datalog.Module = Datalog.Module(
    "Path",
    Seq(),
    Seq(
      nodePattern,
      sevenEdgePattern,
      Datalog.Pattern(
        None,
        "one",
        Seq(
          Datalog.Param("from", Datalog.base.TScalaInt),
          Datalog.Param("to", Datalog.base.TScalaInt)),
        Seq(
          Datalog.Body(Seq(Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))))
        )
      ),
      Datalog.Pattern(
        None,
        "two",
        Seq(
          Datalog.Param("from", Datalog.base.TScalaInt),
          Datalog.Param("to", Datalog.base.TScalaInt)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
              Datalog.Call("one", Seq(Datalog.Var("temp"), Datalog.Var("to")))
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "nodesNotTwoHop",
        Seq(Datalog.Param("x", Datalog.base.TScalaInt), Datalog.Param("y", Datalog.base.TScalaInt)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("node", Seq(Datalog.Var("x"))),
              Datalog.Call("node", Seq(Datalog.Var("y"))),
              Datalog.Call("two", Seq(Datalog.Var("x"), Datalog.Var("y")), neg = true)
            )
          )
        )
      )
    ),
    Seq()
  )

  val comparatorPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "edge",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 2"))
            ),
            Datalog.Compare(Datalog.EqComparator, Datalog.Var("from"), Datalog.Var("to"))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 4"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 4"))
            )
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 3"))
            ),
            Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to"))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Compare(Datalog.EqComparator, Datalog.Var("from"), Datalog.Var("to"))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 2"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Compare(Datalog.NeqComparator, Datalog.Var("to"), Datalog.Var("from"))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 3"))
            ),
            Datalog.Computed(
              Datalog.Var("to"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Compare(Datalog.NeqComparator, Datalog.Var("from"), Datalog.Var("to"))
          )
        )
      )
    )

  val countAggPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "numberOfEdges",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("res", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("res"),
              Datalog.CountAggregation("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
            )
          )
        )
      )
    )

  val countAggPatternCompareWithConst: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "numberOfEdges",
      Seq(Datalog.Param("from", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Constant(Datalog.base.IntLiteral(3)),
              Datalog.CountAggregation("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
            )
          )
        )
      )
    )

  val pathPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        )
      )
    )

  val pathPatternExt: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.ExtensionalCall("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        )
      )
    )

  val pathPatternLeftRecursive: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Call("path", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("edge", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        )
      )
    )

  val pathPatternSwitchBodies: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "path",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("temp"))),
            Datalog.Call("path", Seq(Datalog.Var("temp"), Datalog.Var("to")))
          )
        ),
        Datalog.Body(
          Seq(
            Datalog.Call("edge", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        )
      )
    )

  val notTargetOfPattern: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "notTargetOf",
      Seq(
        Datalog.Param("from", Datalog.base.TScalaInt),
        Datalog.Param("n", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Call("node", Seq(Datalog.Var("from"))),
            Datalog.Call("node", Seq(Datalog.Var("n"))),
            Datalog.Call("path", Seq(Datalog.Var("from"), Datalog.Var("n")), neg = true)
          )
        )
      )
    )

  val query: Datalog.Pattern =
    Datalog.Pattern(
      None,
      "query",
      Seq(Datalog.Param("to", Datalog.base.TScalaInt)),
      Seq(
        Datalog.Body(
          Seq(
            Datalog.Computed(
              Datalog.Var("from"),
              Datalog.Evaluation(Seq(), Datalog.base.TScalaInt, Scala(q"() => 1"))
            ),
            Datalog.Call("path", Seq(Datalog.Var("from"), Datalog.Var("to")))
          )
        )
      )
    )
}
