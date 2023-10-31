package inca.ir.extension.impure

import inca.ir.{Var, *}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{DoubleNum, IntNum}
import inca.ir.extension.data.TData
import inca.ir.extension.demand.TDemand
import inca.ir.typing.IRTypechecker
import inca.util.Gensym
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class ImpureLoweringTest extends AnyFunSuiteLike:
  val baseIR: BaseIR = new BaseIR {}
  val impureIR: IR = new IR with data.IR with arithmetic.IR {}

  val gensym = new Gensym()

  val oidDataDef = data.DataDefinition(Name("OID"), Seq(data.CaseDefinition(Name("OID"), Seq(arithmetic.TInt))))
  val impureAlloc = new ImpurityKind:
    override val name: String = "alloc"
    override val ty: Type = arithmetic.TInt
  def alloc(to: Term): Impure =
    val local = Name(gensym.fresh("currentAlloc"))
    Impure(Var(local),
      Seq(Eq(to, data.Construct("OID", Seq(Var(local))))),
      arithmetic.Add(Var(local), arithmetic.IntNum(1)),
      impureAlloc
    )

  val impureUpdate = new ImpurityKind:
    override val name: String = "update"
    override val ty: Type = arithmetic.TInt


  def module(relations: ModuleEntry*): Module =
    val typecheckerBefore = new IRTypechecker
    val typecheckerAfter = new IRTypechecker
    val lowering = new Lowering {}

    val mod = Module("M", impureIR.language, relations)
    var printedMod = false
    var lowered: Module = null
    try {
      typecheckerBefore.typecheck(mod)
      println(mod)
      printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerAfter.typecheck(lowered)
      lowered
    } finally {
      if (!printedMod)
        println(mod)
      println(lowered)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsAfter = typecheckerAfter.getErrors
      if (errorsBefore.nonEmpty) {
        println("Type errors in original code:")
        errorsBefore.foreach(println)
      }
      if (errorsAfter.nonEmpty) {
        println("Type errors in lowered code:")
        errorsAfter.foreach(println)
      }
    }

  test("Simple lower to BaseIR") {
    val m = module(
      oidDataDef,
      Relation("R",
        Seq(
          Param("a", TData(oidDataDef.name))
        ),
        Seq(
          Body(Seq(
            alloc(Var("t1")),
            alloc(Var("t2")),
            alloc(Var("t3")),
            Eq(Var(Name("a")), Var(Name("t3")))
          ))
        )
      )
    )
    assert(m.relations("R").params.exists(_.name.name == "alloc$0"))
    assert(m.relations("R").params.exists(_.name.name == "alloc$4"))
  }

  test("With aux relation") {
    val impureAlloc = new ImpurityKind:
      override val name: String = "alloc"
      override val ty: Type = arithmetic.TInt

    val m = module(
      oidDataDef,
      Relation("R",
        Seq(
          Param("a", TData(oidDataDef.name))
        ),
        Seq(
          Body(Seq(
            alloc(Var("t1")),
            Call(Name("Q"), Seq(Var(Name("x0")))),
            alloc(Var("t2")),
            Call(Name("Q"), Seq(Var(Name("x0")))),
            alloc(Var("t3")),
            Eq(Var(Name("a")), Var(Name("t3")))
          ))
        )
      ),
      Relation(Name("Q"),
        Seq(Param(Name("i"), arithmetic.TInt)),
        Seq(Body(Seq(
          Eq(Var(Name("i")), arithmetic.IntNum(0))
        )))
      )
    )
    assert(m.relations("R").params.exists(_.name.name == "alloc$0"))
    assert(m.relations("R").params.exists(_.name.name == "alloc$6"))
    assert(m.relations("Q").params.exists(_.name.name == "alloc$0"))
    assert(m.relations("Q").params.exists(_.name.name == "alloc$1"))
  }

  test("multiple impurity kinds") {
    val m = module(
      oidDataDef,
      Relation("R",
        Seq(
          Param("a", TData(oidDataDef.name))
        ),
        Seq(
          Body(Seq(
            alloc(Var("t1")),
            Call(Name("Field"), Seq(Var("t1"), DoubleNum(1.0))),
            Call(Name("Field"), Seq(Var("t1"), DoubleNum(2.0))),
            alloc(Var("t2")),
            Call(Name("Field"), Seq(Var("t2"), DoubleNum(3.0))),
            Call(Name("Field"), Seq(Var("t1"), DoubleNum(4.0))),
            alloc(Var("t3")),
            Eq(Var(Name("a")), Var(Name("t3")))
          ))
        )
      ),
      Relation(Name("Field"),
        Seq(
          Param(Name("o"), TDemand(TData(oidDataDef.name))),
          Param(Name("v"), TDemand(arithmetic.TDouble))
        ),
        Seq(Body(Seq(
          Impure(Var(Name("currentUpdate")),
            Seq(),
            arithmetic.Add(Var(Name("currentUpdate")), IntNum(1)),
            impureUpdate
          )
        )))
      )
    )
    assert(m.relations("R").params.exists(_.name.name == "alloc$0"))
    assert(m.relations("R").params.exists(_.name.name == "alloc$8"))
    assert(m.relations("R").params.exists(_.name.name == "update$0"))
    assert(m.relations("R").params.exists(_.name.name == "update$5"))
    assert(m.relations("Field").params.exists(_.name.name == "alloc$0"))
    assert(m.relations("Field").params.exists(_.name.name == "alloc$1"))
    assert(m.relations("Field").params.exists(_.name.name == "update$0"))
    assert(m.relations("Field").params.exists(_.name.name == "update$2"))
  }
