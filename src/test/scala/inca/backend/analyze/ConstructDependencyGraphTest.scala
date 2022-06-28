package inca.backend.analyze

import inca.backend.analyze.DependencyGraph.PositiveCall
import inca.backend.ir.Datalog._
import org.scalatest.flatspec.AnyFlatSpec

class ConstructDependencyGraphTest extends AnyFlatSpec {

  "Graph" must "be built correctly" in {
    val one = Constant(base.IntLiteral(1))
    val two = Constant(base.IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), two),
          HasType(one, base.TScalaInt),
          Call("TestTP", Seq(), true, false)
        ))
      )),
      Pattern(None, "TestTP", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), two),
          HasType(one, base.TScalaInt),
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


    val graph = new DependencyGraph(module1)
    assertResult(graph.nodes)(Set("foo", "TestTP", "Pattern 2", "foo"))
    assertResult(graph.edges("foo"))(Set(("TestTP", PositiveCall)))
    assertResult(graph.edges("TestTP"))(Set(("TestTP", PositiveCall)))
    assertResult(graph.edges("Pattern 2"))(Set(("TestTP", PositiveCall)))
  }

}
