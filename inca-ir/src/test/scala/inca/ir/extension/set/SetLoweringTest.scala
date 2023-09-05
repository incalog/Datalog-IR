package inca.ir.extension.set

import org.scalatest.funsuite.AnyFunSuite
import inca.ir.{BaseIR, Body, Call, Eq, Language, Module, Neq, Param, Relation, TAny, Type, Var, string2name}
import inca.ir.typing.{CompilationMessage, Typechecker}
import inca.ir.extension.set.*
import inca.ir.extension.block
import inca.ir.extension.disjunction
import inca.ir.extension.data
import inca.ir.extension.arithmetic
import inca.ir.extension.arithmetic.{IntNum, TDouble, TInt}
import inca.util.TupleOps

class SetLoweringTest extends AnyFunSuite {
  case class Failed(messages: Seq[CompilationMessage]) extends Exception(messages.mkString("\n"))

  trait SetWithArithmetic extends IR with arithmetic.IR:
    override val name: String = "SetArithmetic"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object SetWithArithmetic extends SetWithArithmetic {}

  trait Stage1IR extends BaseIR with block.IR with disjunction.IR with data.IR:
    override val name: String = "DisjunctionBlock"
    override def language: Language = super.language
    override def requires: Language = Language(IR)
  object Stage1IR extends Stage1IR {}

  val lowering = Lowering(IR, Stage1IR)
  val blockLowering = block.Lowering(Stage1IR, disjunction.IR)
  val disjunctionLowering = disjunction.Lowering(disjunction.IR, BaseIR)

  def param(i: Int, typ: Type = TAny) = Param("p" + i, typ)
  def setParam(i: Int, typ: Type = TAny) = Param("p" + i, TSet(typ))
  def term(i: Int) = Var("p" + i)

  def lower(mod: Module)(using typechecker: Typechecker): Seq[Module] = {
    def printModule(module: Module) = {
      println(module)
      println()
    }

    val lowerings = Seq(lowering, blockLowering, disjunctionLowering)
    var module = mod
    printModule(module)
    typechecker.typecheck(module)
    typechecker.failOnError()
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
    val mainRelation = Relation("main", Seq(Param("x", TSet(TInt))), Seq(
      Body(Seq(
        Eq(Var("y"), IntNum(1)),
        Eq(Var("z"), Set(Seq(Var("y"), IntNum(2)))),
        Eq(Var("x"), SetUnion(Var("z"), Set.from(IntNum(0), IntNum(2)))),
      ))
    ))
    /*val testRelation = Relation("test", Seq(param(0, TInt), setParam(1, TInt), setParam(2, TInt)), Seq(
      Body(Seq(
      ))
    ))*/

    val mod = Module("Test", SetWithArithmetic.language, Seq(mainRelation))

    lower(mod)
  }
}
