package inca.backend.optimize

import inca.backend.ir.Datalog._
import inca.frontend.constraint.compiler.ConstraintOptions
import inca.runtime.context.{DataModel, QueryScope}
import inca.util.matchers.IncaGPMatchers
import org.scalatest.flatspec.AnyFlatSpec

class TestEliminateAliases extends AnyFlatSpec with IncaGPMatchers {

  val dataModel = new DataModel()
  val scope = new QueryScope(dataModel)
  val options = ConstraintOptions(optimizations = Seq(EliminateAliases, FoldConstantAtoms))

  "eliminateAliases" must "find variable aliases" in {
    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("b")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ), Seq())
    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("c")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ), Seq())
    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ), Seq())
    val module4 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ), Seq())
    val module5 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("b"))
        ))
      ))
    ), Seq())
    val module6 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("c"))
        ))
      ))
    ), Seq())
    val module7 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("b"), Var("p"))
        ))
      ))
    ), Seq())
    val module8 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("c"), Var("p"))
        ))
      ))
    ), Seq())

    val optimized = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
        ))
      ))
    ), Seq())

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
    ), Seq())
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked)
        ))
      ))
    ), Seq())
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
    ), Seq())
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ), Seq())
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
    ), Seq())
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", TAny)), Seq(
        Body(Seq(
          Path(Var("p"), TAnyLinked, NextLink, Var("b"), TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ), Seq())
    assertOptimize(optimized3, module3)
  }

}
