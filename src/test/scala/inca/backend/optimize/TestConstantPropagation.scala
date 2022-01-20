package inca.backend.optimize

import inca.backend.ir.Datalog._
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.Scala
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

import scala.meta._

class TestConstantPropagation extends AnyFlatSpec with IncaGPMatchers {

  val dataModel = new DataModel()
  val scope = new QueryScope(dataModel)
  val options = ConstraintOptions(optimizations = Seq(ConstantPropagation))

  def optimize(module: Module): Module =
    ConstantPropagation.optimizer(dataModel).optimizeModule(module)

  "ConstantPropagation" must "propagate constants" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, Var("b"), one)
        ))
      ))
    ), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
        ))
      ))
    ), Seq())
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, Var("b"), one),
          Compare(EqComparator, Var("c"), two)
        ))
      ))
    ), Seq())
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
        ))
      ))
    ), Seq())
    assertOptimize(optimized2, module2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, Var("b"), one),
          Compare(EqComparator, Var("c"), two),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ), Seq())
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, two)
        ))
      ))
    ), Seq())
    assertOptimize(optimized3, module3)
  }


  "ConstantPropagation" must "propagate constants to Eval" in {
    val one = Constant(IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), one),
          Computed(Var("c"), Evaluation(Seq(Var("b") -> TScalaInt), TScalaBoolean, Scala(q"(x: Int) => x > 1")))
        ))
      ))
    ), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Computed(Var("c"), Evaluation(Seq(), TScalaBoolean, Scala(q"() => 1 > 1")))
        ))
      ))
    ), Seq())
    assertOptimize(optimized1, module1)
  }
}
