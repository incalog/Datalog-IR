package inca.backend.optimize

import inca.IncaMatchers
import inca.backend.ir.GP._
import org.scalatest.flatspec.AnyFlatSpec

class TestConstantConstraintFolding extends AnyFlatSpec with IncaMatchers {

  def optimize(module: Module): Module =
    ConstantConstraintFolding.optimizeModule(module)

  "ConstantPropagation" must "propagate constants" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(2))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one)
        ))
      ))
    ))
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ))
    assert(optimize(module1) == optimized1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two)
        ))
      ))
    ))
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ))
    assert(optimize(module2) == optimized2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two),
          Compare(EqComparator, one, two)
        ))
      ))
    ))
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
      ))
    ))
    assert(optimize(module3) == optimized3)
  }

}
