package inca.backend.optimize

import inca.IncaMatchers
import inca.backend.ir.GP._
import org.scalatest.flatspec.AnyFlatSpec

class TestEliminateAliases extends AnyFlatSpec with IncaMatchers {

  def optimize(module: Module): Module =
    ConstantConstraintFolding.optimizeModule(EliminateAliases.optimizeModule(module))

  "eliminateAliases" must "find variable aliases" in {
    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("b")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("p"), Var("c")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module4 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("c"), Var("p")),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val module5 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("b"))
        ))
      ))
    ))
    val module6 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("p"), Var("c"))
        ))
      ))
    ))
    val module7 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("b"), Var("p"))
        ))
      ))
    ))
    val module8 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Compare(EqComparator, Var("b"), Var("c")),
          Compare(EqComparator, Var("c"), Var("p"))
        ))
      ))
    ))

    val optimized = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
        ))
      ))
    ))

    assert(optimize(module1) == optimized)
    assert(optimize(module2) == optimized)
    assert(optimize(module3) == optimized)
    assert(optimize(module4) == optimized)
    assert(optimize(module5) == optimized)
    assert(optimize(module6) == optimized)
    assert(optimize(module7) == optimized)
    assert(optimize(module8) == optimized)
  }


  "eliminateAliases" must "find path aliases" in {
    val one = Constant(IntLiteral(1))
    val two = Constant(IntLiteral(1))

    val module1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked),
          Path(Var("p"), Var("c"), NextLink, TAnyLinked),
          Compare(EqComparator, Var("b"), Var("c"))
        ))
      ))
    ))
    val optimized1 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked)
        ))
      ))
    ))
    assert(optimize(module1) == optimized1)

    val module2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked),
          Path(Var("p"), Var("c"), NextLink, TAnyLinked),
          HasType(Var("b"), TAnyLinked),
          HasType(Var("c"), TAnyLinked)
        ))
      ))
    ))
    val optimized2 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ))
    assert(optimize(module2) == optimized2)

    val module3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked),
          Path(Var("p"), Var("c"), NextLink, TAnyLinked),
          Path(Var("p"), Var("d"), NextLink, TAnyLinked),
          HasType(Var("b"), TAnyLinked),
          HasType(Var("c"), TAnyLinked),
          HasType(Var("d"), TAnyLinked)
        ))
      ))
    ))
    val optimized3 = Module("Test", Seq(), Seq(
      Pattern(None, "foo", Seq(Param("p", None)), Seq(
        Body(Seq(
          Path(Var("p"), Var("b"), NextLink, TAnyLinked),
          HasType(Var("b"), TAnyLinked)
        ))
      ))
    ))
    assert(optimize(module3) == optimized3)
  }

}
