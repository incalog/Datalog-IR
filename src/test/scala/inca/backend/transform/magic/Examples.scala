package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.GP
import inca.util.Meta.Scala

import scala.meta.quasiquotes._

object Examples {

  def gpmodule(content: GP.Pattern*): GP.Module =
    GP.Module("Main", Seq(), content, Seq())

  val incFunGP = GP.Pattern(None, "inc", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val incMainGP = GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  incMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = GP.Pattern(None, "fact", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out"), GP.Var("lit_0"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  ))))
  val factMainGP = GP.Pattern(None, "main", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  factMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val factModuleGP = gpmodule(factFunGP, factMainGP)

  val adornedIncFunGP = GP.Pattern(None, "inc_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  )))).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedIncMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  )))).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedIncModuleGP = gpmodule(adornedIncFunGP, adornedIncMainGP)

  val magicIncFunGP = GP.Pattern(None, "inc_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("input_inc_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval"))
  ))))
  val magicIncMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Call("inc_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val magicInputIncFunGP = GP.Pattern(None, "input_inc_bf", Seq(GP.Param("n", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 0"))),
    GP.Eq(GP.Var("lit"), GP.Var("n"))
  ))))
  val magicIncModuleGP = gpmodule(magicIncFunGP, magicIncMainGP, magicInputIncFunGP)



  val magicFactFunGP = GP.Pattern(None, "fact_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out"), GP.Var("lit_0"))
  )), GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact_bf", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  ))))
  val magicFactMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  ))))
  val magicInputFactFunGP = GP.Pattern(None, "input_fact_bf", Seq(GP.Param("n_0", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Call("input_fact_bf", Seq(GP.Var("n")), transitive = false, neg = false),
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Eq(GP.Var("eval_0"), GP.Var("n_0"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Eq(GP.Var("lit"), GP.Var("n_0"))
  ))))
  val magicFactModuleGP = gpmodule(magicFactFunGP, magicFactMainGP, magicInputFactFunGP)


  val adornedFactFunGP = GP.Pattern(None, "fact_bf", Seq(GP.Param("n", GP.TScalaInt), GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.True),
    GP.Computed(GP.Var("lit_0"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Eq(GP.Var("out"), GP.Var("lit_0"))
  )), GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit") -> GP.TScalaInt),
      GP.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    GP.Eq(GP.Var("eval"), GP.False),
    GP.Computed(GP.Var("lit_1"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 1"))),
    GP.Computed(GP.Var("eval_0"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("lit_1") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    GP.Call("fact_bf", Seq(GP.Var("eval_0"), GP.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    GP.Computed(GP.Var("eval_1"), GP.Evaluation(Seq(GP.Var("n") -> GP.TScalaInt, GP.Var("out_0") -> GP.TScalaInt),
      GP.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    GP.Eq(GP.Var("out"), GP.Var("eval_1"))
  )))).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedFactMainGP = GP.Pattern(None, "main_f", Seq(GP.Param("out", GP.TScalaInt)), Seq(GP.Body(Seq(
    GP.Computed(GP.Var("lit"), GP.Evaluation(Seq(), GP.TScalaInt, Scala(q"() => 3"))),
    GP.Call("fact_bf", Seq(GP.Var("lit"), GP.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    GP.Eq(GP.Var("out"), GP.Var("out_0"))
  )))).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedFactModuleGP = gpmodule(adornedFactFunGP, adornedFactMainGP)


  val tLink = GP.TNode("Link")
  val fromTLink = GP.NamedLink(tLink, "from")
  val toTLink = GP.NamedLink(tLink, "to")
  val tNode = GP.TNode("Node")

  val reachableGP = GP.Pattern(None, "reachable", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
    Seq(
      GP.Body(Seq(
        GP.HasType(GP.Var("link"), tLink),
        GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
        GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
      )),
      GP.Body(Seq(
        GP.HasType(GP.Var("link"), tLink),
        GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
        GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
        GP.Call("reachable", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false)
      )),
    )
  )
  val nodeGP = GP.Pattern(None, "node", Seq(GP.Param("x", tNode)),
    Seq(
      GP.Body(Seq(
        GP.HasType(GP.Var("link"), tLink),
        GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
      )),
      GP.Body(Seq(
        GP.HasType(GP.Var("link"), tLink),
        GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
      )),
    )
  )
  val unreachableGP = GP.Pattern(None, "unreachable", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
    Seq(
      GP.Body(Seq(
        GP.Call("node", Seq(GP.Var("x")), transitive = false, neg = false),
        GP.Call("node", Seq(GP.Var("y")), transitive = false, neg = false),
        GP.Call("reachable", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true),
      ))
    )
  )
  val unreachableMainGP = GP.Pattern(None, "main", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
    Seq(
      GP.Body(Seq(
        GP.HasType(GP.Var("y"), tNode),
        GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
        GP.Call("unreachable", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
      ))
    )
  )
  unreachableMainGP.addHint(MagicSetHints.Main(Seq(false, false)))
  val unreachableModuleGP = gpmodule(unreachableGP, nodeGP, reachableGP, unreachableMainGP)

  val adornedUnreachableModuleGP = GP.Module("Main", Seq(),
    Seq(
      GP.Pattern(None, "reachable_bb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Call("reachable_bb", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, true)))
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(true, true))),
      GP.Pattern(None, "node_b", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(true))),
      GP.Pattern(None, "node_f", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(false))),
      GP.Pattern(None, "unreachable_fb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true).addHint(MagicSetHints.Adornment(Seq(true, true))),
          ))
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, true))),
      GP.Pattern(None, "main_ff", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Call("unreachable_fb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(false, true))),
          ))
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, false))),
    ),
    Seq())

  val magicUnreachableModuleGP = GP.Module("Main", Seq(),
    Seq(
      GP.Pattern(None, "reachable_bb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("y"), tNode)
          )),
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Call("reachable_bb", Seq(GP.Var("z"), GP.Var("y")), transitive = false, neg = false)
          )),
        )
      ),
      GP.Pattern(None, "node_b", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_node_b", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.Call("input_node_b", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "node_f", Seq(GP.Param("x", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
          )),
          GP.Body(Seq(
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("x"), tNode),
          )),
        )
      ),
      GP.Pattern(None, "unreachable_fb", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = true),
          ))
        )
      ),
      GP.Pattern(None, "main_ff", Seq(GP.Param("x", tNode), GP.Param("y", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Call("unreachable_fb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
          ))
        )
      ),
      GP.Pattern(None, "input_unreachable_fb", Seq(GP.Param("y_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.HasType(GP.Var("y"), tNode),
            GP.Eq(GP.Var("y"), GP.Constant(GP.IntLiteral(0))), // this is only temporary
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          ))
        )
      ),
      GP.Pattern(None, "input_reachable_bb", Seq(GP.Param("x_0", tNode), GP.Param("y_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_reachable_bb", Seq(GP.Var("x"), GP.Var("y")), transitive = false, neg = false),
            GP.HasType(GP.Var("link"), tLink),
            GP.Path(GP.Var("link"), tNode, fromTLink, GP.Var("x"), tNode),
            GP.Path(GP.Var("link"), tNode, toTLink, GP.Var("z"), tNode),
            GP.Eq(GP.Var("z"), GP.Var("x_0")),
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          )),
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Call("node_b", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Eq(GP.Var("x"), GP.Var("x_0")),
            GP.Eq(GP.Var("y"), GP.Var("y_0"))
          ))
        )
      ),
      GP.Pattern(None, "input_node_b", Seq(GP.Param("x_0", tNode)),
        Seq(
          GP.Body(Seq(
            GP.Call("input_unreachable_fb", Seq(GP.Var("y")), transitive = false, neg = false),
            GP.Call("node_f", Seq(GP.Var("x")), transitive = false, neg = false),
            GP.Eq(GP.Var("y"), GP.Var("x_0"))
          ))
        )
      ),
    ),
    Seq())
}
