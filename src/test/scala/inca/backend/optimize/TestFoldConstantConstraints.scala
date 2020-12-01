package inca.backend.optimize

import inca.IncaMatchers
import inca.backend.ir.GP._
import inca.compiler.Options
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.scalatest.flatspec.AnyFlatSpec

class TestFoldConstantConstraints extends AnyFlatSpec with IncaMatchers {

  val langMeta = new LanguageMetaInfo()
  val scope = new QueryScope(langMeta)
  val options = Options(langMeta, optimizations = Seq(FoldConstantConstraints))

  "ConstantPropagation" must "propagate constants" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(2))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one)
        ))
      ))
    ), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ), Seq())
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two)
        ))
      ))
    ), Seq())
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one)
        ))
      ))
    ), Seq())
    assertOptimize(optimized2, module2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two),
          Compare(EqComparator, one, two)
        ))
      ))
    ), Seq())
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TScala("Any"))), Seq(
      ))
    ), Seq())
    assertOptimize(optimized3, module3)
  }

}
