package inca.backend.optimize

import inca.backend.ir.Datalog._
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

class TestFoldConstantAtoms extends AnyFlatSpec with IncaGPMatchers {

  val dataModel = new DataModel()
  val scope = new QueryScope(dataModel)
  val options = ConstraintOptions(optimizations = Seq(FoldConstantAtoms))

  "ConstantPropagation" must "propagate constants" in {
    val one = Constant(base.IntLiteral(1))
    val two = Constant(base.IntLiteral(2))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one)
        ))
      ))
    ), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ), Seq())
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two)
        ))
      ))
    ), Seq())
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ), Seq())
    assertOptimize(optimized2, module2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two),
          Compare(EqComparator, one, two)
        ))
      ))
    ), Seq())
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", base.TScalaBoolean)), Seq(
      ))
    ), Seq())
    assertOptimize(optimized3, module3)
  }

}
