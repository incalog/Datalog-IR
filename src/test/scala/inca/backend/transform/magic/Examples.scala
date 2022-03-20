package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.util.Scala
import scala.meta.quasiquotes._

object Examples {

  def gpmodule(content: Datalog.Pattern*): Datalog.Module =
    Datalog.Module("Main", Seq(), content, Seq())

  val incFunGP = Datalog.Pattern(
    None,
    "inc",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left + right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval"))
        )
      )
    )
  )
  val incMainGP = Datalog.Pattern(
    None,
    "main",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))
          ),
          Datalog.Call(
            "inc",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  )
  incMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val incModuleGP = gpmodule(incFunGP, incMainGP)

  val factFunGP = Datalog.Pattern(
    None,
    "fact",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.True),
          Datalog.Computed(
            Datalog.Var("lit_0"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("lit_0"))
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.False),
          Datalog.Computed(
            Datalog.Var("lit_1"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval_0"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit_1") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left - right")
            )
          ),
          Datalog.Call(
            "fact",
            Seq(Datalog.Var("eval_0"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Computed(
            Datalog.Var("eval_1"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("out_0") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left * right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval_1"))
        )
      )
    )
  )
  val factMainGP = Datalog.Pattern(
    None,
    "main",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))
          ),
          Datalog.Call(
            "fact",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  )
  factMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val factModuleGP = gpmodule(factFunGP, factMainGP)

  val adornedIncFunGP = Datalog.Pattern(
    None,
    "inc_bf",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left + right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval"))
        )
      )
    )
  ).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedIncMainGP = Datalog.Pattern(
    None,
    "main_f",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))
          ),
          Datalog.Call(
            "inc_bf",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ).addHint(MagicSetHints.Adornment(Seq(true, false))),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  ).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedIncModuleGP = gpmodule(adornedIncFunGP, adornedIncMainGP)

  val magicIncFunGP = Datalog.Pattern(
    None,
    "inc_bf",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Call("input$inc_bf", Seq(Datalog.Var("n")), transitive = false, neg = false),
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left + right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval"))
        )
      )
    )
  )
  val magicIncMainGP = Datalog.Pattern(
    None,
    "main_f",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))
          ),
          Datalog.Call(
            "inc_bf",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  )
  val magicInputIncFunGP = Datalog.Pattern(
    None,
    "input$inc_bf",
    Seq(Datalog.Param("n", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 0"))
          ),
          Datalog.Eq(Datalog.Var("lit"), Datalog.Var("n"))
        )
      )
    )
  )
  val magicIncModuleGP = gpmodule(magicIncFunGP, magicIncMainGP, magicInputIncFunGP)

  val magicFactFunGP = Datalog.Pattern(
    None,
    "fact_bf",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Call("input$fact_bf", Seq(Datalog.Var("n")), transitive = false, neg = false),
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.True),
          Datalog.Computed(
            Datalog.Var("lit_0"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("lit_0"))
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.Call("input$fact_bf", Seq(Datalog.Var("n")), transitive = false, neg = false),
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.False),
          Datalog.Computed(
            Datalog.Var("lit_1"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval_0"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit_1") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left - right")
            )
          ),
          Datalog.Call(
            "fact_bf",
            Seq(Datalog.Var("eval_0"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Computed(
            Datalog.Var("eval_1"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("out_0") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left * right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval_1"))
        )
      )
    )
  )
  val magicFactMainGP = Datalog.Pattern(
    None,
    "main_f",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))
          ),
          Datalog.Call(
            "fact_bf",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  )
  val magicInputFactFunGP = Datalog.Pattern(
    None,
    "input$fact_bf",
    Seq(Datalog.Param("n_0", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Call("input$fact_bf", Seq(Datalog.Var("n")), transitive = false, neg = false),
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.False),
          Datalog.Computed(
            Datalog.Var("lit_1"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval_0"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit_1") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left - right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval_0"), Datalog.Var("n_0"))
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))
          ),
          Datalog.Eq(Datalog.Var("lit"), Datalog.Var("n_0"))
        )
      )
    )
  )
  val magicFactModuleGP = gpmodule(magicFactFunGP, magicFactMainGP, magicInputFactFunGP)

  val adornedFactFunGP = Datalog.Pattern(
    None,
    "fact_bf",
    Seq(Datalog.Param("n", Datalog.TScalaInt), Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.True),
          Datalog.Computed(
            Datalog.Var("lit_0"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("lit_0"))
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit") -> Datalog.TScalaInt),
              Datalog.TScalaBoolean,
              Scala(q"(left: Int, right: Int) => left == right")
            )
          ),
          Datalog.Eq(Datalog.Var("eval"), Datalog.False),
          Datalog.Computed(
            Datalog.Var("lit_1"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 1"))
          ),
          Datalog.Computed(
            Datalog.Var("eval_0"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("lit_1") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left - right")
            )
          ),
          Datalog.Call(
            "fact_bf",
            Seq(Datalog.Var("eval_0"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ).addHint(MagicSetHints.Adornment(Seq(true, false))),
          Datalog.Computed(
            Datalog.Var("eval_1"),
            Datalog.Evaluation(
              Seq(Datalog.Var("n") -> Datalog.TScalaInt, Datalog.Var("out_0") -> Datalog.TScalaInt),
              Datalog.TScalaInt,
              Scala(q"(left: Int, right: Int) => left * right")
            )
          ),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("eval_1"))
        )
      )
    )
  ).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedFactMainGP = Datalog.Pattern(
    None,
    "main_f",
    Seq(Datalog.Param("out", Datalog.TScalaInt)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Computed(
            Datalog.Var("lit"),
            Datalog.Evaluation(Seq(), Datalog.TScalaInt, Scala(q"() => 3"))
          ),
          Datalog.Call(
            "fact_bf",
            Seq(Datalog.Var("lit"), Datalog.Var("out_0")),
            transitive = false,
            neg = false
          ).addHint(MagicSetHints.Adornment(Seq(true, false))),
          Datalog.Eq(Datalog.Var("out"), Datalog.Var("out_0"))
        )
      )
    )
  ).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedFactModuleGP = gpmodule(adornedFactFunGP, adornedFactMainGP)

  val tLink = Datalog.TNode("Link")
  val fromTLink = Datalog.NamedLink(tLink, "from")
  val toTLink = Datalog.NamedLink(tLink, "to")
  val tNode = Datalog.TNode("Node")

  val reachableGP = Datalog.Pattern(
    None,
    "reachable",
    Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.HasType(Datalog.Var("link"), tLink),
          Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
          Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("y"), tNode)
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.HasType(Datalog.Var("link"), tLink),
          Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
          Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("z"), tNode),
          Datalog.Call(
            "reachable",
            Seq(Datalog.Var("z"), Datalog.Var("y")),
            transitive = false,
            neg = false
          )
        )
      )
    )
  )
  val nodeGP = Datalog.Pattern(
    None,
    "node",
    Seq(Datalog.Param("x", tNode)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.HasType(Datalog.Var("link"), tLink),
          Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode)
        )
      ),
      Datalog.Body(
        Seq(
          Datalog.HasType(Datalog.Var("link"), tLink),
          Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("x"), tNode)
        )
      )
    )
  )
  val unreachableGP = Datalog.Pattern(
    None,
    "unreachable",
    Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.Call("node", Seq(Datalog.Var("x")), transitive = false, neg = false),
          Datalog.Call("node", Seq(Datalog.Var("y")), transitive = false, neg = false),
          Datalog.Call(
            "reachable",
            Seq(Datalog.Var("x"), Datalog.Var("y")),
            transitive = false,
            neg = true
          )
        )
      )
    )
  )
  val unreachableMainGP = Datalog.Pattern(
    None,
    "main",
    Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
    Seq(
      Datalog.Body(
        Seq(
          Datalog.HasType(Datalog.Var("y"), tNode),
          Datalog.Eq(
            Datalog.Var("y"),
            Datalog.Constant(Datalog.IntLiteral(0))
          ), // this is only temporary
          Datalog.Call(
            "unreachable",
            Seq(Datalog.Var("x"), Datalog.Var("y")),
            transitive = false,
            neg = false
          )
        )
      )
    )
  )
  unreachableMainGP.addHint(MagicSetHints.Main(Seq(false, false)))
  val unreachableModuleGP = gpmodule(unreachableGP, nodeGP, reachableGP, unreachableMainGP)

  val adornedUnreachableModuleGP = Datalog.Module(
    "Main",
    Seq(),
    Seq(
      Datalog.Pattern(
        None,
        "reachable_bb",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("y"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("z"), tNode),
              Datalog.Call(
                "reachable_bb",
                Seq(Datalog.Var("z"), Datalog.Var("y")),
                transitive = false,
                neg = false
              ).addHint(MagicSetHints.Adornment(Seq(true, true)))
            )
          )
        )
      ).addHint(MagicSetHints.Adornment(Seq(true, true))),
      Datalog.Pattern(
        None,
        "node_b",
        Seq(Datalog.Param("x", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("x"), tNode)
            )
          )
        )
      ).addHint(MagicSetHints.Adornment(Seq(true))),
      Datalog.Pattern(
        None,
        "node_f",
        Seq(Datalog.Param("x", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("x"), tNode)
            )
          )
        )
      ).addHint(MagicSetHints.Adornment(Seq(false))),
      Datalog.Pattern(
        None,
        "unreachable_fb",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("node_f", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.Call("node_b", Seq(Datalog.Var("y")), transitive = false, neg = false),
              Datalog.Call(
                "reachable_bb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = true
              ).addHint(MagicSetHints.Adornment(Seq(true, true)))
            )
          )
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, true))),
      Datalog.Pattern(
        None,
        "main_ff",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("y"), tNode),
              Datalog.Eq(
                Datalog.Var("y"),
                Datalog.Constant(Datalog.IntLiteral(0))
              ), // this is only temporary
              Datalog.Call(
                "unreachable_fb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = false
              ).addHint(MagicSetHints.Adornment(Seq(false, true)))
            )
          )
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, false)))
    ),
    Seq()
  )

  val magicUnreachableModuleGP = Datalog.Module(
    "Main",
    Seq(),
    Seq(
      Datalog.Pattern(
        None,
        "reachable_bb",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$reachable_bb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("y"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$reachable_bb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("z"), tNode),
              Datalog.Call(
                "reachable_bb",
                Seq(Datalog.Var("z"), Datalog.Var("y")),
                transitive = false,
                neg = false
              )
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "node_b",
        Seq(Datalog.Param("x", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call("input$node_b", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Call("input$node_b", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("x"), tNode)
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "node_f",
        Seq(Datalog.Param("x", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode)
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("x"), tNode)
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "unreachable_fb",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$unreachable_fb",
                Seq(Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.Call("node_f", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.Call("node_b", Seq(Datalog.Var("y")), transitive = false, neg = false),
              Datalog.Call(
                "reachable_bb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = true
              )
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "main_ff",
        Seq(Datalog.Param("x", tNode), Datalog.Param("y", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("y"), tNode),
              Datalog.Eq(
                Datalog.Var("y"),
                Datalog.Constant(Datalog.IntLiteral(0))
              ), // this is only temporary
              Datalog.Call(
                "unreachable_fb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = false
              )
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "input$unreachable_fb",
        Seq(Datalog.Param("y_0", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.HasType(Datalog.Var("y"), tNode),
              Datalog.Eq(
                Datalog.Var("y"),
                Datalog.Constant(Datalog.IntLiteral(0))
              ), // this is only temporary
              Datalog.Eq(Datalog.Var("y"), Datalog.Var("y_0"))
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "input$reachable_bb",
        Seq(Datalog.Param("x_0", tNode), Datalog.Param("y_0", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$reachable_bb",
                Seq(Datalog.Var("x"), Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.HasType(Datalog.Var("link"), tLink),
              Datalog.Path(Datalog.Var("link"), tNode, fromTLink, Datalog.Var("x"), tNode),
              Datalog.Path(Datalog.Var("link"), tNode, toTLink, Datalog.Var("z"), tNode),
              Datalog.Eq(Datalog.Var("z"), Datalog.Var("x_0")),
              Datalog.Eq(Datalog.Var("y"), Datalog.Var("y_0"))
            )
          ),
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$unreachable_fb",
                Seq(Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.Call("node_f", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.Call("node_b", Seq(Datalog.Var("y")), transitive = false, neg = false),
              Datalog.Eq(Datalog.Var("x"), Datalog.Var("x_0")),
              Datalog.Eq(Datalog.Var("y"), Datalog.Var("y_0"))
            )
          )
        )
      ),
      Datalog.Pattern(
        None,
        "input$node_b",
        Seq(Datalog.Param("x_0", tNode)),
        Seq(
          Datalog.Body(
            Seq(
              Datalog.Call(
                "input$unreachable_fb",
                Seq(Datalog.Var("y")),
                transitive = false,
                neg = false
              ),
              Datalog.Call("node_f", Seq(Datalog.Var("x")), transitive = false, neg = false),
              Datalog.Eq(Datalog.Var("y"), Datalog.Var("x_0"))
            )
          )
        )
      )
    ),
    Seq()
  )
}
