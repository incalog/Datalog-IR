package inca.ir.extension.set

import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{BaseIR, Body, Call, Eq, Language, Module, Neq, Param, Relation, TAny, Type, Var, string2name}
import inca.ir.typing.{CompilationMessage, IRTypechecker, Typechecker}
import inca.ir.extension.set.*
import inca.ir.extension.*
import inca.ir.extension.arithmetic.{IntNum, TDouble, TInt}
import inca.ir.extension.demand.TDemand
import inca.ir.lowering.BaseLowering
import inca.util.TupleOps

class SetLoweringTest extends AnyFunSuite {
  val lowering = new Lowering {}

  def module(relations: Relation*): Module =
    val typecheckerBefore = new IRTypechecker

    val setLowering = Seq(
      new Lowering {},
      new disjunction.Lowering {},
      new tuple.Lowering {},
      new block.Lowering {},
      new demand.Lowering {}
    )

    def lower(l: BaseLowering, m: Module): Module =
      val checker = new IRTypechecker
      try checker.typecheck(m)
      finally checker.getErrors.foreach(println)
      println(s"Lowering ${l.loweredIRs}")
      val lowered = l.lower(m)
      println(lowered)
      lowered

    val mod = Module("M", Language(IR, arithmetic.IR), relations)
    typecheckerBefore.typecheck(mod)
    println(mod)
    val lowered = setLowering.foldLeft(mod)((mod, l) => lower(l, mod))
    lowered

  test("Set literal") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(Param("x", TSet(TInt))), Seq(
      Body(Seq(
        Eq(Var("y"), IntNum(0)),
        Eq(Var("x"), SetLit(Seq(Var("y"), IntNum(1), IntNum(2))))
      ))
    ))

    val mod = module(mainRelation)

  }

  /*test("Set union Test no refun") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("z"), Set(Seq(Var("y"), term(2)))),
        //Eq(Var("a"), Set.from(term(0), term(1))),
        // TODO: Test this as arg: Set.from(term(0), term(1))
        Eq(Var("x"), SetUnion(Var("z"), Set.from(term(0), term(2)))),
        //Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2))))),
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))
    val testRelation = Relation("test", Seq(param(0), setParam(1), setParam(2)), Seq(
      Body(Seq(
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation, testRelation))

    lower(mod)
  }*/

  /*test("Set union Test") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("z"), Set(Seq(Var("y"), term(2)))),
        //Eq(Var("a"), Set.from(term(0), term(1))),
        // TODO: Test this as arg: Set.from(term(0), term(1))
        Eq(Var("x"), SetUnion(Var("z"), Set.from(term(0), term(2))))),
        //Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2))))),
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))
    val testRelation = Relation("test", Seq(param(0), setParam(1), setParam(2)), Seq(
      Body(Seq(
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation, testRelation))

    lower(mod)
  }*/

  /*test("Set refunctionalize call") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("z"), Set(Seq(Var("y"), term(2)))),
        Eq(Var("a"), Set.from(term(0), term(1))),
        Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2)))))),
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))
    val testRelation = Relation("test", Seq(param(0), setParam(1)), setParam(2)), Seq(
      Body(Seq(
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation, testRelation))

    lower(mod)
  }*/

  /*test("Set Member") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam), Seq(
      Body(Seq(
        Eq(Var("a"), Set.from(term(0), term(1))),
        // Test this as arg: Set.from(term(0), term(1))
        SetMember(Var("a"), term(0))
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation))

    lower(mod)
  }*/

  test("Set with arithmetic") {
    implicit val typechecker = new Typechecker { }
    val mainRelation = Relation("main", Seq(Param("x", TInt)), Seq(
      Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Eq(Var("z"), SetLit(Seq(Var("y"), IntNum(2)))),
        SetMember(Var("x"), SetUnion(Var("z"), SetLit.from(IntNum(0), IntNum(2)))),
      ))
    ))
    /*val testRelation = Relation("test", Seq(param(0, TInt), setParam(1, TInt), setParam(2, TInt)), Seq(
      Body(Seq(
      ))
    ))*/

    val mod = module(mainRelation)

  }

  test("Set with arithmetic 2") {
    implicit val typechecker = new Typechecker {}
    val mainRelation = Relation("main", Seq(Param("x", TDemand(TSet(TInt))), Param("s", TSet(TInt))), Seq(
      Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Eq(Var("z"), SetLit(Seq(Var("y"), IntNum(2)))),
        Eq(Var("s"), SetUnion(Var("x"), SetUnion(Var("z"), SetLit.from(IntNum(0), IntNum(2))))),
      ))
    ))
    /*val testRelation = Relation("test", Seq(param(0, TInt), setParam(1, TInt), setParam(2, TInt)), Seq(
      Body(Seq(
      ))
    ))*/

    val mod = module(mainRelation)

  }
}
