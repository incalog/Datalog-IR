package inca.backend.optimize

import inca.backend.ir.GP._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestFoldConstantConstraints extends AnyFlatSpec with IncaMatchers {

  val langMeta = new LanguageMetaInfo()
  val scope = new QueryScope(langMeta)
  val options = CompilerOptions(langMeta, optimizations = Seq(FoldConstantConstraints))

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
    assertOptimize(optimized1, module1)

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
    assertOptimize(optimized2, module2)

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
    assertOptimize(optimized3, module3)
  }

}
