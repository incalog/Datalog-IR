package inca.ir.extension.locals

import inca.ir.extension.*
import inca.ir.extension.arithmetic.{Add, DoubleNum, IntNum, TInt}
import inca.ir.extension.data.TData
import inca.ir.extension.demand.TDemand
import inca.ir.extension.disjunction.{ Disjunction, Lowering as DisjunctionLowering }
import inca.ir.typing.IRTypechecker
import inca.ir.{Var, *}
import inca.util.Gensym
import org.scalatest.funsuite.AnyFunSuiteLike

import scala.collection.immutable.Seq

class LocalsLoweringTest extends AnyFunSuiteLike:
  val baseIR: BaseIR = new BaseIR {}
  val localsIR: IR = new IR with data.IR with arithmetic.IR with disjunction.IR {}

  val gensym = new Gensym()

  def module(relations: ModuleEntry*): Module =
    val typecheckerBefore = new IRTypechecker
    val typecheckerAfter = new IRTypechecker
    val lowering = new Lowering {}
    val disjunctionLowering = new DisjunctionLowering {}
    lowering.isClosedWorld = true

    val mod = Module("M", localsIR.language, relations)
    //var printedMod = false
    var lowered: Module = null
    try {
      typecheckerBefore.checkProgram(Seq(mod))
      //println(mod)
      //printedMod = true
      lowered = lowering.visitProgram(Seq(mod)).head
      typecheckerAfter.checkProgram(Seq(lowered))
      lowered = disjunctionLowering.visitProgram(Seq(lowered)).head
      typecheckerAfter.checkProgram(Seq(lowered))
      lowered
    } finally {
      //if (!printedMod)
      //  println(mod)
      //println(lowered)
      val errorsBefore = typecheckerBefore.getErrors
      val errorsAfter = typecheckerAfter.getErrors
      //if (errorsBefore.nonEmpty) {
        //println("Type errors in original code:")
        //errorsBefore.foreach(println)
      //}
      //if (errorsAfter.nonEmpty) {
        //println("Type errors in lowered code:")
        //errorsAfter.foreach(println)
      //}
    }

  test("Simple lower to BaseIR") {
    val m = module(
      Relation("R",
        Seq(
          Param("x", TInt)
        ),
        Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(1)),
            Assign(Var("a"), Add(Var("a"), IntNum(2))),
            Eq(Var("x"), Var("a"))
          ))
        )
      )
    )
    val vars = m.relations("R").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    assert((0 until 1).forall(i => vars.contains(s"a_$i")))
  }

  test("Lower two bodies") {
    val m = module(
      Relation("R",
        Seq(
          Param("x", TInt)
        ),
        Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(1)),
            Assign(Var("a"), Add(Var("a"), IntNum(2))),
            Eq(Var("x"), Var("a"))
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(2)),
            Assign(Var("a"), Add(Var("a"), IntNum(2))),
            Assign(Var("a"), IntNum(4)),
            Eq(Var("x"), Var("a"))
          ))
        )
      )
    )
    val varsInBody1 = m.relations("R").bodies.head.atoms.flatMap(_.vars).map(_.ref.name.name)
    assert((0 until 1).forall(i => varsInBody1.contains(s"a_$i")))
    val varsInBody2 = m.relations("R").bodies.last.atoms.flatMap(_.vars).map(_.ref.name.name)
    assert((0 until 2).forall(i => varsInBody2.contains(s"a_$i")))
  }

  test("Lower Disjunction") {
    val m = module(
      Relation("R",
        Seq(
          Param("x", TInt)
        ),
        Seq(
          Body(Seq(
            Disjunction(
              Body(Seq(
                Eq(Var("a"), IntNum(1)),
                Assign(Var("a"), Add(Var("a"), IntNum(2))),
              )),
              Body(Seq(
                Eq(Var("a"), IntNum(2)),
                Assign(Var("a"), Add(Var("a"), IntNum(2))),
                Assign(Var("a"), Add(Var("a"), IntNum(3))),
                Assign(Var("a"), Add(Var("a"), IntNum(5))),
              ))
            ),
            Eq(Var("x"), Var("a"))
          )),
        )
      )
    )
    val vars = m.relations("R").bodies.flatMap(_.atoms.flatMap(_.vars)).map(_.ref.name.name)
    assert((1 until 3).forall(i => vars.contains(s"a_$i")))
  }
