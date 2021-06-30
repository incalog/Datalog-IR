package inca.backend.transform.magic

import inca.backend.hints.MagicSetHints
import inca.backend.ir.IR
import inca.util.Scala

import scala.meta.quasiquotes._

object Examples {

  def gpmodule(content: IR.Pattern*): IR.Module =
    IR.Module("Main", Seq(), content, Seq())

  val incFunGP = IR.Pattern(None, "inc", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval"))
  ))))
  val incMainGP = IR.Pattern(None, "main", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Call("inc", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  ))))
  incMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val incModuleGP = gpmodule(incFunGP, incMainGP)




  val factFunGP = IR.Pattern(None, "fact", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.True),
    IR.Computed(IR.Var("lit_0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Eq(IR.Var("out"), IR.Var("lit_0"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.False),
    IR.Computed(IR.Var("lit_1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval_0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit_1") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    IR.Call("fact", Seq(IR.Var("eval_0"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Computed(IR.Var("eval_1"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("out_0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval_1"))
  ))))
  val factMainGP = IR.Pattern(None, "main", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Call("fact", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  ))))
  factMainGP.addHint(MagicSetHints.Main(Seq(false)))
  val factModuleGP = gpmodule(factFunGP, factMainGP)

  val adornedIncFunGP = IR.Pattern(None, "inc_bf", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval"))
  )))).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedIncMainGP = IR.Pattern(None, "main_f", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Call("inc_bf", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  )))).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedIncModuleGP = gpmodule(adornedIncFunGP, adornedIncMainGP)

  val magicIncFunGP = IR.Pattern(None, "inc_bf", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Call("input$inc_bf", Seq(IR.Var("n")), transitive = false, neg = false),
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left + right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval"))
  ))))
  val magicIncMainGP = IR.Pattern(None, "main_f", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Call("inc_bf", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  ))))
  val magicInputIncFunGP = IR.Pattern(None, "input$inc_bf", Seq(IR.Param("n", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 0"))),
    IR.Eq(IR.Var("lit"), IR.Var("n"))
  ))))
  val magicIncModuleGP = gpmodule(magicIncFunGP, magicIncMainGP, magicInputIncFunGP)



  val magicFactFunGP = IR.Pattern(None, "fact_bf", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Call("input$fact_bf", Seq(IR.Var("n")), transitive = false, neg = false),
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.True),
    IR.Computed(IR.Var("lit_0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Eq(IR.Var("out"), IR.Var("lit_0"))
  )), IR.Body(Seq(
    IR.Call("input$fact_bf", Seq(IR.Var("n")), transitive = false, neg = false),
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.False),
    IR.Computed(IR.Var("lit_1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval_0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit_1") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    IR.Call("fact_bf", Seq(IR.Var("eval_0"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Computed(IR.Var("eval_1"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("out_0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval_1"))
  ))))
  val magicFactMainGP = IR.Pattern(None, "main_f", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Call("fact_bf", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  ))))
  val magicInputFactFunGP = IR.Pattern(None, "input$fact_bf", Seq(IR.Param("n_0", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Call("input$fact_bf", Seq(IR.Var("n")), transitive = false, neg = false),
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.False),
    IR.Computed(IR.Var("lit_1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval_0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit_1") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    IR.Eq(IR.Var("eval_0"), IR.Var("n_0"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Eq(IR.Var("lit"), IR.Var("n_0"))
  ))))
  val magicFactModuleGP = gpmodule(magicFactFunGP, magicFactMainGP, magicInputFactFunGP)


  val adornedFactFunGP = IR.Pattern(None, "fact_bf", Seq(IR.Param("n", IR.TScalaInt), IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.True),
    IR.Computed(IR.Var("lit_0"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Eq(IR.Var("out"), IR.Var("lit_0"))
  )), IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit") -> IR.TScalaInt),
      IR.TScalaBoolean, Scala(q"(left: Int, right: Int) => left == right"))),
    IR.Eq(IR.Var("eval"), IR.False),
    IR.Computed(IR.Var("lit_1"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 1"))),
    IR.Computed(IR.Var("eval_0"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("lit_1") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left - right"))),
    IR.Call("fact_bf", Seq(IR.Var("eval_0"), IR.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    IR.Computed(IR.Var("eval_1"), IR.Evaluation(Seq(IR.Var("n") -> IR.TScalaInt, IR.Var("out_0") -> IR.TScalaInt),
      IR.TScalaInt, Scala(q"(left: Int, right: Int) => left * right"))),
    IR.Eq(IR.Var("out"), IR.Var("eval_1"))
  )))).addHint(MagicSetHints.Adornment(Seq(true, false)))
  val adornedFactMainGP = IR.Pattern(None, "main_f", Seq(IR.Param("out", IR.TScalaInt)), Seq(IR.Body(Seq(
    IR.Computed(IR.Var("lit"), IR.Evaluation(Seq(), IR.TScalaInt, Scala(q"() => 3"))),
    IR.Call("fact_bf", Seq(IR.Var("lit"), IR.Var("out_0")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, false))),
    IR.Eq(IR.Var("out"), IR.Var("out_0"))
  )))).addHint(MagicSetHints.Adornment(Seq(false)))
  val adornedFactModuleGP = gpmodule(adornedFactFunGP, adornedFactMainGP)


  val tLink = IR.TNode("Link")
  val fromTLink = IR.NamedLink(tLink, "from")
  val toTLink = IR.NamedLink(tLink, "to")
  val tNode = IR.TNode("Node")

  val reachableGP = IR.Pattern(None, "reachable", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
    Seq(
      IR.Body(Seq(
        IR.HasType(IR.Var("link"), tLink),
        IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
        IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("y"), tNode)
      )),
      IR.Body(Seq(
        IR.HasType(IR.Var("link"), tLink),
        IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
        IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("z"), tNode),
        IR.Call("reachable", Seq(IR.Var("z"), IR.Var("y")), transitive = false, neg = false)
      )),
    )
  )
  val nodeGP = IR.Pattern(None, "node", Seq(IR.Param("x", tNode)),
    Seq(
      IR.Body(Seq(
        IR.HasType(IR.Var("link"), tLink),
        IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
      )),
      IR.Body(Seq(
        IR.HasType(IR.Var("link"), tLink),
        IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("x"), tNode),
      )),
    )
  )
  val unreachableGP = IR.Pattern(None, "unreachable", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
    Seq(
      IR.Body(Seq(
        IR.Call("node", Seq(IR.Var("x")), transitive = false, neg = false),
        IR.Call("node", Seq(IR.Var("y")), transitive = false, neg = false),
        IR.Call("reachable", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = true),
      ))
    )
  )
  val unreachableMainGP = IR.Pattern(None, "main", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
    Seq(
      IR.Body(Seq(
        IR.HasType(IR.Var("y"), tNode),
        IR.Eq(IR.Var("y"), IR.Constant(IR.IntLiteral(0))), // this is only temporary
        IR.Call("unreachable", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false),
      ))
    )
  )
  unreachableMainGP.addHint(MagicSetHints.Main(Seq(false, false)))
  val unreachableModuleGP = gpmodule(unreachableGP, nodeGP, reachableGP, unreachableMainGP)

  val adornedUnreachableModuleGP = IR.Module("Main", Seq(),
    Seq(
      IR.Pattern(None, "reachable_bb", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("y"), tNode)
          )),
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("z"), tNode),
            IR.Call("reachable_bb", Seq(IR.Var("z"), IR.Var("y")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(true, true)))
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(true, true))),
      IR.Pattern(None, "node_b", Seq(IR.Param("x", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
          )),
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("x"), tNode),
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(true))),
      IR.Pattern(None, "node_f", Seq(IR.Param("x", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
          )),
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("x"), tNode),
          )),
        )
      ).addHint(MagicSetHints.Adornment(Seq(false))),
      IR.Pattern(None, "unreachable_fb", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("node_f", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.Call("node_b", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Call("reachable_bb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = true).addHint(MagicSetHints.Adornment(Seq(true, true))),
          ))
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, true))),
      IR.Pattern(None, "main_ff", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("y"), tNode),
            IR.Eq(IR.Var("y"), IR.Constant(IR.IntLiteral(0))), // this is only temporary
            IR.Call("unreachable_fb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false).addHint(MagicSetHints.Adornment(Seq(false, true))),
          ))
        )
      ).addHint(MagicSetHints.Adornment(Seq(false, false))),
    ),
    Seq())

  val magicUnreachableModuleGP = IR.Module("Main", Seq(),
    Seq(
      IR.Pattern(None, "reachable_bb", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("input$reachable_bb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false),
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("y"), tNode)
          )),
          IR.Body(Seq(
            IR.Call("input$reachable_bb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false),
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("z"), tNode),
            IR.Call("reachable_bb", Seq(IR.Var("z"), IR.Var("y")), transitive = false, neg = false)
          )),
        )
      ),
      IR.Pattern(None, "node_b", Seq(IR.Param("x", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("input$node_b", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
          )),
          IR.Body(Seq(
            IR.Call("input$node_b", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("x"), tNode),
          )),
        )
      ),
      IR.Pattern(None, "node_f", Seq(IR.Param("x", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
          )),
          IR.Body(Seq(
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("x"), tNode),
          )),
        )
      ),
      IR.Pattern(None, "unreachable_fb", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("input$unreachable_fb", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Call("node_f", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.Call("node_b", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Call("reachable_bb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = true),
          ))
        )
      ),
      IR.Pattern(None, "main_ff", Seq(IR.Param("x", tNode), IR.Param("y", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("y"), tNode),
            IR.Eq(IR.Var("y"), IR.Constant(IR.IntLiteral(0))), // this is only temporary
            IR.Call("unreachable_fb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false),
          ))
        )
      ),
      IR.Pattern(None, "input$unreachable_fb", Seq(IR.Param("y_0", tNode)),
        Seq(
          IR.Body(Seq(
            IR.HasType(IR.Var("y"), tNode),
            IR.Eq(IR.Var("y"), IR.Constant(IR.IntLiteral(0))), // this is only temporary
            IR.Eq(IR.Var("y"), IR.Var("y_0"))
          ))
        )
      ),
      IR.Pattern(None, "input$reachable_bb", Seq(IR.Param("x_0", tNode), IR.Param("y_0", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("input$reachable_bb", Seq(IR.Var("x"), IR.Var("y")), transitive = false, neg = false),
            IR.HasType(IR.Var("link"), tLink),
            IR.Path(IR.Var("link"), tNode, fromTLink, IR.Var("x"), tNode),
            IR.Path(IR.Var("link"), tNode, toTLink, IR.Var("z"), tNode),
            IR.Eq(IR.Var("z"), IR.Var("x_0")),
            IR.Eq(IR.Var("y"), IR.Var("y_0"))
          )),
          IR.Body(Seq(
            IR.Call("input$unreachable_fb", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Call("node_f", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.Call("node_b", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Eq(IR.Var("x"), IR.Var("x_0")),
            IR.Eq(IR.Var("y"), IR.Var("y_0"))
          ))
        )
      ),
      IR.Pattern(None, "input$node_b", Seq(IR.Param("x_0", tNode)),
        Seq(
          IR.Body(Seq(
            IR.Call("input$unreachable_fb", Seq(IR.Var("y")), transitive = false, neg = false),
            IR.Call("node_f", Seq(IR.Var("x")), transitive = false, neg = false),
            IR.Eq(IR.Var("y"), IR.Var("x_0"))
          ))
        )
      ),
    ),
    Seq())
}
