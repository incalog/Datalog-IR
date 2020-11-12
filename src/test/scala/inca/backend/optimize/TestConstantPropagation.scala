package inca.backend.optimize

import inca.IncaMatchers
import inca.backend.ir.GP._
import inca.compiler.Options
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import org.scalatest.flatspec.AnyFlatSpec

import scala.meta._

class TestConstantPropagation extends AnyFlatSpec with IncaMatchers {

  val langMeta = new LanguageMetaInfo()
  val scope = new QueryScope(langMeta)
  val options = Options(langMeta, optimizations = Seq(ConstantPropagation))

  def optimize(module: Module): Module =
    ConstantPropagation.optimizer(langMeta).optimizeModule(module)

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
    ), Seq(), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one)
        ))
      ))
    ), Seq(), Seq())
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, Var("b"), one),
          Compare(EqComparator, Var("c"), two)
        ))
      ))
    ), Seq(), Seq())
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two)
        ))
      ))
    ), Seq(), Seq())
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
    ), Seq(), Seq())
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two),
          Compare(EqComparator, one, two)
        ))
      ))
    ), Seq(), Seq())
    assertOptimize(optimized3, module3)
  }


  "ConstantPropagation" must "propagate constants to Eval" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val code = q"(x: Int) => x > 1"
    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), one),
          Computed(Var("c"), Evaluation(Seq(Var("b") -> TScalaInt), TScalaBoolean, code))
        ))
      ))
    ), Seq(), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, one, one),
          Computed(Var("c"), Evaluation(Seq(one -> TScalaInt), TScalaBoolean, code))
        ))
      ))
    ), Seq(), Seq())
    assertOptimize(optimized1, module1)
  }
}
