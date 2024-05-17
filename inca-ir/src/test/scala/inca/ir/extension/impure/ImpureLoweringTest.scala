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

  val oidDataDef = data.DataDefinition(Name("OID"))
  val oidCaseDef = data.CaseDefinition(Name("mkOID"), Seq(arithmetic.TInt), TData(oidDataDef.name))
  val impureAlloc = new ImpurityKind:
    override val name: String = "alloc"
    override val ty: Type = arithmetic.TInt
  def alloc(to: Term): Impure =
    val local = Name(gensym.fresh("currentAlloc"))
    Impure(RefByName(local),
      Seq(Eq(to, data.Construct("mkOID", Seq(Var(local))))),
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
    lowering.isClosedWorld = true

    val mod = Module("M", impureIR.language, relations)
    //var printedMod = false
    var lowered: Module = null
    try {
      typecheckerBefore.checkProgram(Seq(mod))
      //println(mod)
      //printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerAfter.checkProgram(Seq(lowered))
      lowered
    } finally {
      //if (!printedMod)
      //  println(mod)
      //println(lowered)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsAfter = typecheckerAfter.getErrors
      if (errorsBefore.nonEmpty) {
        //println("Type errors in original code:")
        //errorsBefore.foreach(println)
      }
      if (errorsAfter.nonEmpty) {
        //println("Type errors in lowered code:")
        //errorsAfter.foreach(println)
      }
    }

  test("Simple lower to BaseIR") {
    val m = module(
      oidDataDef,
      oidCaseDef,
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
    val vars = m.relations("R").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    assert((1 until 4).forall(i => vars.contains(s"alloc$$$i")))
  }

  test("With aux relation") {
    val impureAlloc = new ImpurityKind:
      override val name: String = "alloc"
      override val ty: Type = arithmetic.TInt

    val m = module(
      oidDataDef,
      oidCaseDef,
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

    val rVars = m.relations("R").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    val qVars = m.relations("Q").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    assert((1 until 5).forall(i => rVars.contains(s"alloc$$$i")))
    assert(!qVars.exists(_.startsWith("alloc")))
  }

  test("multiple impurity kinds") {
    val m = module(
      oidDataDef,
      oidCaseDef,
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
          Impure(RefByName(Name("currentUpdate")),
            Seq(),
            arithmetic.Add(Var(Name("currentUpdate")), IntNum(1)),
            impureUpdate
          )
        )))
      )
    )

    val rVars = m.relations("R").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    val fieldVars = m.relations("Field").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    assert((1 until 5).forall(i => rVars.contains(s"alloc$$$i")))
    assert((1 until 6).forall(i => rVars.contains(s"update$$$i")))
    assert(!fieldVars.exists(_.startsWith("alloc")))
    assert((1 until 3).forall(i => fieldVars.contains(s"update$$$i")))
  }
