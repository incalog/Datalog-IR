package inca.backend.analyze

import inca.backend.ir.Datalog._
import org.scalatest.flatspec.AnyFlatSpec

class ConstructDependencyGraphTest extends AnyFlatSpec {

  "Graph" must "be built correctly" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), two),
          HasType(one, TScalaInt),
          Call("TestTP", Seq(), true, false)
        ))
      )),
      Pattern(None, "TestTP", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), two),
          HasType(one, TScalaInt),
          Call("TestTP", Seq(), true, false)
        ))
      )),
      Pattern(None, "Pattern 2", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Call("TestTP", Seq(), true, false)
        ))
      )),
    ), Seq())


    val graph = ConstructDependencyGraph(module1)
    val pats = module1.pats.map { p => p.name -> p }.toMap
    assertResult(graph.nodes)(Set(pats("foo"), pats("TestTP"), pats("Pattern 2"), pats("foo")))
    assertResult(graph.edges(pats("foo")))(Set((pats("TestTP"), false)))
    assertResult(graph.edges(pats("TestTP")))(Set((pats("TestTP"), false)))
    assertResult(graph.edges(pats("Pattern 2")))(Set((pats("TestTP"), false)))
  }

}
