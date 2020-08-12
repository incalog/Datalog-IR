package inca.backend.optimize

import inca.backend.ir.GP._
import inca.runtime.context.{LanguageMetaInfo, QueryScope}
import inca.{CompilerOptions, IncaMatchers}
import org.scalatest.flatspec.AnyFlatSpec

class TestEliminateAliases extends AnyFlatSpec with IncaMatchers {

  val langMeta = new LanguageMetaInfo()
  val scope = new QueryScope(langMeta)
  val options = CompilerOptions(langMeta, optimizations = Seq(EliminateAliases, FoldConstantConstraints))

  "eliminateAliases" must "find variable aliases" in {
    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("b")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("c")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module4 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module5 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("b"))
        ))
      ))
    ))
    val module6 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("c"))
        ))
      ))
    ))
    val module7 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("b"), Var("p"))
        ))
      ))
    ))
    val module8 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("c"), Var("p"))
        ))
      ))
    ))

    val optimized = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
        ))
      ))
    ))

    assertOptimize(optimized, module1)
    assertOptimize(optimized, module2)
    assertOptimize(optimized, module3)
    assertOptimize(optimized, module4)
    assertOptimize(optimized, module5)
    assertOptimize(optimized, module6)
    assertOptimize(optimized, module7)
    assertOptimize(optimized, module8)
  }


  "eliminateAliases" must "find path aliases" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          Path(Var("p"), TAnyLinked, NextLink, Var("c"), TAnyLinked),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked)
        ))
      ))
    ))
    assertOptimize(optimized1, module1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          Path(Var("p"), TAnyLinked, NextLink, Var("c"), TAnyLinked),
          HasType(Var("b"), TAnyLinked),
          HasType(Var("c"), TAnyLinked)
        ))
      ))
    ))
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ))
    assertOptimize(optimized2, module2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          Path(Var("p"), TAnyLinked, NextLink, Var("c"), TAnyLinked),
          Path(Var("p"), TAnyLinked, NextLink, Var("d"), TAnyLinked),
          HasType(Var("b"), TAnyLinked),
          HasType(Var("c"), TAnyLinked),
          HasType(Var("d"), TAnyLinked)
        ))
      ))
    ))
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ))
    assertOptimize(optimized3, module3)
  }

}
