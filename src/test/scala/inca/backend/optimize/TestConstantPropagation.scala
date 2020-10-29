package inca.backend.optimize

import inca.backend.ir.GP._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

import scala.meta._

class TestConstantPropagation extends AnyFlatSpec with IncaMatchers {

  val langMeta = new LanguageMetaInfo()
  val scope = new QueryScope(langMeta)
  val options = CompilerOptions(langMeta, optimizations = Seq(ConstantPropagation))

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
    ))
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one)
        ))
      ))
    ))
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, Var("b"), one),
          Compare(EqComparator, Var("c"), two)
        ))
      ))
    ))
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two)
        ))
      ))
    ))
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
    ))
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), one),
          Compare(EqComparator, one, one),
          Compare(EqComparator, two, two),
          Compare(EqComparator, one, two)
        ))
      ))
    ))
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
          Computed(Var("c"), Evaluation(Seq(Var("b") -> TInt), TBool, code))
        ))
      ))
    ))
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, one, one),
          Computed(Var("c"), Evaluation(Seq(one -> TInt), TBool, code))
        ))
      ))
    ))
    assertOptimize(optimized1, module1)
  }
}
