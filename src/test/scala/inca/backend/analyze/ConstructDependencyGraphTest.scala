package inca.backend.analyze

import inca.backend.analyze.DependencyGraph.PositiveCall
import inca.backend.ir.DatalogScala._
import org.scalatest.flatspec.AnyFlatSpec

class ConstructDependencyGraphTest extends AnyFlatSpec {

  import host._

  "Graph" must "be built correctly" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module(
      "Test",
      Seq(),
      Seq(
        Pattern(
          None,
          "foo",
          Seq(Param("p", TAny)),
          Seq(
            Body(
              Seq(
                Compare(EqComparator, Var("c"), two),
                HasType(one, TScalaInt),
                Call("TestTP", Seq(), true, false)
              )
            )
          )
        ),
        Pattern(
          None,
          "TestTP",
          Seq(Param("p", TAny)),
          Seq(
            Body(
              Seq(
                Compare(EqComparator, Var("c"), two),
                HasType(one, TScalaInt),
                Call("TestTP", Seq(), true, false)
              )
            )
          )
        ),
        Pattern(
          None,
          "Pattern 2",
          Seq(Param("p", TAny)),
          Seq(
            Body(
              Seq(
                Compare(EqComparator, Var("p"), one),
                Call("TestTP", Seq(), true, false)
              )
            )
          )
        )
      ),
      Seq()
    )

    val graph = new DependencyGraph(module1)
    assertResult(graph.nodes)(Set("foo", "TestTP", "Pattern 2", "foo"))
    assertResult(graph.edges("foo"))(Set(("TestTP", PositiveCall)))
    assertResult(graph.edges("TestTP"))(Set(("TestTP", PositiveCall)))
    assertResult(graph.edges("Pattern 2"))(Set(("TestTP", PositiveCall)))
  }

}
