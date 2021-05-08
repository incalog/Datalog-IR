package inca.backend.analyze


import inca.backend.ir.Datalog._
import org.scalatest.flatspec.AnyFlatSpec

class StratificationAnalysisTest extends AnyFlatSpec  {


  "Graph" must "be stratifiable" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module("Test2", Seq(), Seq(
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

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Call("bar", Seq(), true, false)
        ))
      )),
      Pattern(None, "bar", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Call("foo", Seq(), true, false)
        ))
      ))
    ), Seq())

    val module3 = Module("M", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Call("bar", Seq(), true, false),
        ))
      )),
      Pattern(None, "bar", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Call("baz", Seq(), true, true),
        ))
      )),
      Pattern(None, "baz", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Call("foo", Seq(), true, false),
        ))
      )),
    ), Seq())

    assert(!StratificationAnalysis.hasNegCycle(module1))
    assert(!StratificationAnalysis.hasNegCycle(module2))
    assert(StratificationAnalysis.hasNegCycle(module3))
  }
}
