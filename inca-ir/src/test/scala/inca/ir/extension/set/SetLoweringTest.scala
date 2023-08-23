package inca.ir.extension.set

import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{Module, Relation, Param, Body, Var, TAny, Eq, Neq, Call, Language, BaseIR, string2name}
import inca.ir.typing.{CompilationMessage, Typechecker}
import inca.ir.extension.set.*
import inca.ir.extension.block
import inca.ir.extension.disjunction
import inca.ir.extension.data

class SetLoweringTest extends AnyFunSuite {
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  val typechecker = new Typechecker {}

  trait Stage1IR extends BaseIR with block.IR with disjunction.IR with data.IR:
    override val name: String = "DisjunctionBlock"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object Stage1IR extends Stage1IR {}

  val lowering = Lowering(IR, Stage1IR)
  val blockLowering = block.Lowering(Stage1IR, disjunction.IR)
  val disjunctionLowering = disjunction.Lowering(disjunction.IR, BaseIR)

  def stopIfNeeded(): Unit = {
    val errors = typechecker.getErrors
    if (errors.nonEmpty)
      throw Failed(errors)
  }


  def param(i: Int) = Param("p" + i, TAny)
  def setParam(i: Int) = Param("p" + i, TSet(TAny))
  def term(i: Int) = Var("p" + i)

  def lower(mod: Module): Seq[Module] = {
    def typecheck(module: Module): Unit = {
      typechecker.typecheck(module)
      stopIfNeeded()
    }

    def printModule(module: Module) = {
      println(module)
      println()
    }

    val lowerings = Seq(lowering, blockLowering, disjunctionLowering)
    var module = mod
    printModule(module)
    typecheck(module)
    printModule(module)
    println("----------------")

    lowerings.map { lr =>
      module = lr.lower(module)
      //printModule(module)
      //typecheck(module)
      printModule(module)
      println("----------------")
      module
    }
  }

  /*test("Set create Test") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("x"), Set(Seq(Var("y"), term(1), term(2))))
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation))

    lower(mod)
  }*/

  /*test("Set union Test") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam.addHint(Hints.Refunctionalize())), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("z"), Set(Seq(Var("y"), term(2)))),
        //Eq(Var("a"), Set.from(term(0), term(1))),
        // TODO: Test this as arg: Set.from(term(0), term(1))
        Eq(Var("x"), SetUnion(Var("z"), Set.from(term(0), term(2)))).addHint(Hints.Refunctionalize(Seq("x"))),
        //Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2))))).addHint(Hints.Refunctionalize),
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

  test("Set refunctionalize call") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam.addHint(Hints.Refunctionalize())), Seq(
      Body(Seq(
        Eq(Var("y"), term(0)),
        Eq(Var("z"), Set(Seq(Var("y"), term(2)))),
        Eq(Var("a"), Set.from(term(0), term(1))),
        Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2))))).addHint(Hints.Refunctionalize()),
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))
    val testRelation = Relation("test", Seq(param(0), setParam(1).addHint(Hints.Refunctionalize()), setParam(2).addHint(Hints.Refunctionalize())), Seq(
      Body(Seq(
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation, testRelation))

    lower(mod)
  }

  /*test("Set Member") {
    val outParam = Param("x", TSet(TAny))
    val mainRelation = Relation("main", Seq(param(0), param(1), param(2), outParam.addHint(Hints.Refunctionalize())), Seq(
      Body(Seq(
        Eq(Var("a"), Set.from(term(0), term(1))),
        // TODO: Test this as arg: Set.from(term(0), term(1))
        Call("test", Seq(Var("y"), Var("a"), SetUnion(Var("z"), Set.from(term(0), term(2))))).addHint(Hints.Refunctionalize()),
        //Eq(Var("w"), SetIntersection(Var("z"), Set.from(term(0), term(2)))),
        //Eq(Var("v"), SetIntersection(Set.from(term(0), term(1)), Set.from(term(0), term(2))))
      ))
    ))

    val mod = Module("Test", IR.language, Seq(mainRelation))

    lower(mod)
  }*/
}
