package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.analysis.ValueNumbering


class ClonesTest extends AnyFunSuite {

  def performTest(expected: IRModule, input: IRModule): Unit = {
    val VN = new ValueNumbering
    val typechecker = new Typechecker {}  // TODO move typechecker into ValueNumbering ?
    typechecker.checkModule(input)
    println(s"before VN: \n$input")
    val result = VN.valueNumbering(input)
    typechecker.checkModule(result)
    println(s"after VN: \n$result")
    assertResult(expected)(result)
  }

  test("Simple Redundant term in Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(1)),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            //Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq with Add"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            Eq(Var(Name("H2")), Add(Var("X"), IntNum(2))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq with Sub") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(1), IntNum(2))),
            Eq(Var(Name("Y")), Add(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Sub(Var("Y"), IntNum(2))),
            Eq(Var(Name("H2")), Sub(Var("X"), IntNum(2))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(1), IntNum(2))),
            //Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("H1")), Sub(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq with Mul and Div") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(2))),
            Eq(Var(Name("Y")), Mul(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Div(Var("Y"), IntNum(3))),
            Eq(Var(Name("H2")), Div(Var("X"), IntNum(3))),
            Eq(Var(Name("Z")), Add(Var("H1"), Mul(Var("H2"), IntNum(3)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(2))),
            //Eq(Var(Name("Y")), Mul(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Div(Var("X"), IntNum(3))),
            //Eq(Var(Name("H2")), Div(Var("X"), IntNum(3))),
            Eq(Var(Name("Z")), Add(Var("H1"), Mul(Var("H1"), IntNum(3)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq with Remainder,Min,Max,Abs") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Remainder(IntNum(1), IntNum(2))),
            Eq(Var(Name("Y")), Remainder(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Min(Var("Y"), IntNum(2))),
            Eq(Var(Name("H2")), Min(Var("X"), IntNum(2))),
            Eq(Var(Name("H3")), Max(Var("Y"), IntNum(2))),
            Eq(Var(Name("H4")), Max(Var("X"), IntNum(2))),
            Eq(Var(Name("H5")), Abs(Var("Y"))),
            Eq(Var(Name("H6")), Abs(Var("X"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Remainder(IntNum(1), IntNum(2)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Remainder(IntNum(1), IntNum(2))),
//            Eq(Var(Name("Y")), Remainder(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Min(Var("X"), IntNum(2))),
//            Eq(Var(Name("H2")), Min(Var("X"), IntNum(2))),
            Eq(Var(Name("H3")), Max(Var("X"), IntNum(2))),
//            Eq(Var(Name("H4")), Max(Var("X"), IntNum(2))),
            Eq(Var(Name("H5")), Abs(Var("X"))),
//            Eq(Var(Name("H6")), Abs(Var("X"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Redundant term in Eq nested") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H1")), Div(Mul(Var("Y"), Var("X")), IntNum(2))),
            Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
            Eq(Var(Name("H3")), Sub(Var("H1"), Mul(Var("H2"), IntNum(3)))),
            Eq(Var(Name("Z")), Sub(Var("H2"), Mul(IntNum(1), Add(IntNum(2), IntNum(3))))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            //Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H1")), Div(Mul(Var("X"), Var("X")), IntNum(2))),
            //Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
            Eq(Var(Name("H3")), Sub(Var("H1"), Mul(Var("H1"), IntNum(3)))),
            Eq(Var(Name("Z")), Sub(Var("H1"), Var(Name("X")))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq: Two Bodies") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H1")), Div(Mul(Var("Y"), Var("X")), IntNum(2))),
            Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
            Eq(Var(Name("Z")), Sub(Var("H1"), Mul(Var("H2"), IntNum(3)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          )),
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(2), IntNum(2))),
            Eq(Var(Name("Y")), Mul(IntNum(2), IntNum(2))),
            Eq(Var(Name("A")), Div(Var(Name("X")), Var(Name("Y")))),
            Eq(Var(Name("B")), Div(Mul(Var("Y"), Var("X")), IntNum(2))),  // should not be replaced with H1 from other body
            Eq(Var(Name("Z")), Add(Var(Name("X")), Add(Var(Name("A")),Var(Name("B"))))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            //Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H1")), Div(Mul(Var("X"), Var("X")), IntNum(2))),
            //Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
            Eq(Var(Name("Z")), Sub(Var("H1"), Mul(Var("H1"), IntNum(3)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          )),
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(2), IntNum(2))),
            //Eq(Var(Name("Y")), Mul(IntNum(2), IntNum(2))),
            Eq(Var(Name("A")), Div(Var(Name("X")), Var(Name("X")))),
            Eq(Var(Name("B")), Div(Mul(Var("X"), Var("X")), IntNum(2))), // should not be replaced with H1 from other body
            Eq(Var(Name("Z")), Add(Var(Name("X")), Add(Var(Name("A")), Var(Name("B"))))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq with more Eqs with same Var") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(1)),
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(1))),
            Eq(Var(Name("Z1")), Mul(IntNum(1), IntNum(1))),
            Eq(Var(Name("Z2")), IntNum(1)),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Z1")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            //Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(1))),  // TODO gets remembered correctly but could be removed here
//            Eq(Var(Name("Z1")), Mul(IntNum(1), IntNum(1)))),
//            Eq(Var(Name("Z2")), IntNum(1)),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant Eqs") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), IntNum(1)),
            Eq(Var(Name("B")), IntNum(1)),
            Eq(Var(Name("C")), Var("B")),
            Eq(Var(Name("D")), Var("A")),
            Eq(Var(Name("C")), Var("D")),
            Eq(Var(Name("E")), IntNum(1)),
            Eq(Var(Name("C")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("C"))),
            Eq(Var(Name("param$1")), Var(Name("D")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), IntNum(1)),
//            Eq(Var(Name("B")), IntNum(1)),
//            Eq(Var(Name("C")), Var("B")),
//            Eq(Var(Name("D")), Var("A")),
//            Eq(Var(Name("C")), Var("D")),
//            Eq(Var(Name("E")), IntNum(1)),
//            Eq(Var(Name("C")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant Eqs 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), IntNum(1)),
            Eq(Var(Name("B")), IntNum(1)),
            Eq(Var(Name("C")), Var("B")),
            Eq(Var(Name("D")), Var("A")),
            Eq(Var(Name("C")), Var("D")),
            Eq(Var(Name("E")), Mul(IntNum(1), IntNum(1))),  // TODO remove even if Equality known later? But its actually already known here... Would that always be the case (with Calls etc)
            Eq(Var(Name("A")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("C"))),
            Eq(Var(Name("param$1")), Var(Name("E")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), IntNum(1)),
            //            Eq(Var(Name("B")), IntNum(1)),
            //            Eq(Var(Name("C")), Var("B")),
            //            Eq(Var(Name("D")), Var("A")),
            //            Eq(Var(Name("C")), Var("D")),
            //            Eq(Var(Name("C")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in LT") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Y")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Z")), Mul(IntNum(2), IntNum(1))),
            LT(Var(Name("Z")), Add(IntNum(2), IntNum(1))),
            LT(Mul(IntNum(2), IntNum(1)), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            //Eq(Var(Name("Y")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Z")), Mul(IntNum(2), IntNum(1))),
            LT(Var(Name("Z")), Var(Name("X"))),
            LT(Var(Name("Z")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Redundant term in Eq with GE, LE, LT & Neq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(3)),
            Eq(Var(Name("Y")), Add(Var(Name("X")),IntNum(1))),
            Eq(Var(Name("Z")), IntNum(3)),
            Eq(Var(Name("H1")), Div(Var(Name("X")), IntNum(2))),
            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(2))),
            LT(Var(Name("H1")), Add(Var(Name("X")),IntNum(1))), // would var be bound before? -> typechecker says yes
            GE(Mul(Var(Name("X")),IntNum(1)), Var(Name("H1"))),
            Eq(Var(Name("Y")), Var(Name("Z")), true),
            Eq(Mul(Var(Name("X")), IntNum(2)), Var(Name("H1")), true),
            Eq(Var(Name("X")), Div(Var(Name("X")), IntNum(2)), true),
            LE(Var(Name("Y")), Add(Var(Name("X")),IntNum(1))),     // this is redundant...
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(3)),
            Eq(Var(Name("Y")), Add(Var(Name("X")), IntNum(1))),
//            Eq(Var(Name("Z")), IntNum(1)),
            Eq(Var(Name("H1")), Div(Var(Name("X")), IntNum(2))),
            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(2))),
            LT(Var(Name("H1")), Var(Name("Y"))),
            GE(Mul(Var(Name("X")), IntNum(1)), Var(Name("H1"))),
            Eq(Var(Name("Y")), Var(Name("X")), true),
            Eq(Var(Name("H2")), Var(Name("H1")), true),
            Eq(Var(Name("X")), Var(Name("H1")), true),
            LE(Var(Name("Y")), Var(Name("Y"))),             // this is redundant... -> TODO remove LE/GE(X,X)
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("comparison that should not be removed") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
            Eq(Var(Name("Z")), Add(IntNum(2), IntNum(1))),
            Eq(Var("Y"), Var("Z")),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
//            Eq(Var(Name("Z")), Var(Name("X"))),
            Eq(Var("Y"), Var("X")),     // TODO this gets removed but should stay since it makes relation empty
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call replace Args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("C")), Var(Name("B"))),
            Call(Name("b"), Seq(TermArg(Var(Name("A"))), TermArg(Var(Name("B"))), TermArg(Var(Name("C")))), false),
            Call(Name("b"), Seq(TermArg(IntNum(1)), TermArg(Var(Name("B"))), TermArg(Var(Name("B")))), false),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("C")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(5)),
            Eq(Var(Name("param$2")), IntNum(5)),
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5)),
            Eq(Var(Name("param$1")), IntNum(5)),
            Eq(Var(Name("param$2")), IntNum(5)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
//            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
//            Eq(Var(Name("C")), Var(Name("B"))),
            Call(Name("b"), Seq(TermArg(Var(Name("A"))), TermArg(Var(Name("A"))), TermArg(Var(Name("A")))), false),
            Call(Name("b"), Seq(TermArg(IntNum(1)), TermArg(Var(Name("A"))), TermArg(Var(Name("A")))), false),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(5)),
            Eq(Var(Name("param$2")), IntNum(5)),
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5)),
            Eq(Var(Name("param$1")), IntNum(5)),
            Eq(Var(Name("param$2")), IntNum(5)),
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Simple Redundant Var bound in Call") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"),Seq(TermArg(Var(Name("X")))),false),
            Call(Name("b"),Seq(TermArg(Var(Name("Y")))),false),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X")))), false),
            //Call(Name("b"), Seq(TermArg(Var(Name("Y"))), false),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant Var bound in Call & replace Args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            Call(Name("b"), Seq(TermArg(Var(Name("X"))),TermArg(Var(Name("B")))), false),
            Call(Name("b"), Seq(TermArg(Var(Name("Y"))),TermArg(Var(Name("B")))), false),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
            //Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            Call(Name("b"), Seq(TermArg(Var(Name("X"))), TermArg(Var(Name("A")))), false),
            //Call(Name("b"), Seq(TermArg(Var(Name("Y"))), TermArg(Var(Name("B")))), false),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Add (Commutativity)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            Eq(Var(Name("H2")), Add(IntNum(2), Var("X"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Add (Associativity)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H2")), Add(Add(Var("X"), IntNum(2)), IntNum(3))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(2)),
            Eq(Var(Name("H1")), Add(Var("X"), Add(IntNum(2), IntNum(3)))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Add zero") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("H1")), Add(IntNum(1), IntNum(0))),
            Eq(Var(Name("H2")), Add(IntNum(0), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            //Eq(Var(Name("H1")), Var(Name("X"))),
            //Eq(Var(Name("H2")), Var(Name("X"))),
            Eq(Var(Name("Y")), Add(Var("X"), Var("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("sub not commutative") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Sub(IntNum(1), Var(Name("X")))),
            Eq(Var(Name("H2")), Sub(Var(Name("X")), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Sub(IntNum(1), Var(Name("X")))),
            Eq(Var(Name("H2")), Sub(Var(Name("X")), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("mul commutative") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Mul(IntNum(1), Var(Name("X")))),
            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Mul(IntNum(1), Var(Name("X")))),
            //Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("mul associative") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(3)),
            Eq(Var(Name("H1")), Mul(IntNum(2), Mul(Var(Name("X")), IntNum(3)))),
            Eq(Var(Name("H2")), Mul(Mul(IntNum(2), Var(Name("X"))), IntNum(3))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(3)),
            Eq(Var(Name("H1")), Mul(IntNum(2), Mul(Var(Name("X")), IntNum(3)))),
            //Eq(Var(Name("H2")), Mul(Mul(IntNum(2), Var(Name("X"))), IntNum(3))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("mul & add distributivity") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("H1")), Mul(IntNum(2), Add(Var(Name("X")), IntNum(3)))),
            Eq(Var(Name("H2")), Add(Mul(IntNum(2), Var(Name("X"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("H1")), Mul(IntNum(2), Add(Var(Name("X")), IntNum(3)))),
            //Eq(Var(Name("H2")), Add(Mul(IntNum(2), Var(Name("X"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("mul one and zero") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Mul(Var(Name("X")), IntNum(0))),
            Eq(Var(Name("H2")), IntNum(0)),
            Eq(Var(Name("H3")), Mul(IntNum(0), Var(Name("X")))),
            Eq(Var(Name("Y")), Add(Var("H2"), Var("H3"))),
            Eq(Var(Name("Z")), Mul(Var("X"), IntNum(1))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Mul(Var(Name("X")), IntNum(0))),
//            Eq(Var(Name("H2")), IntNum(0)),
//            Eq(Var(Name("H3")), Mul(IntNum(0), Var(Name("X")))),
//            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),  // 0 + 0 -> 0 -> Var("H1") -> can be removed too
//            Eq(Var(Name("Z")), Mul(Var("X"), IntNum(1))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("H1"))),
            Eq(Var(Name("param$2")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("div") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Div(IntNum(2), IntNum(1))),
            Eq(Var(Name("H1")), IntNum(2)),
            Eq(Var(Name("H2")), Div(IntNum(4), IntNum(2))),
            Eq(Var(Name("H3")), Div(IntNum(1), IntNum(2))),
            Eq(Var(Name("H4")), Div(Var("H1"), Var("H3"))),
            Eq(Var(Name("H5")), Mul(Var("X"), Var("X"))),
            Eq(Var(Name("H6")), Div(Var("X"), IntNum(1))),
            Eq(Var(Name("H7")), Div(Add(Var("X"), IntNum(0)), IntNum(2))),
            Eq(Var(Name("H8")), Add(Div(Var("X"), IntNum(2)), Div(IntNum(0), IntNum(2)))),
            Eq(Div(Var("X"), IntNum(2)), Var("H8")),
            Eq(Var("Y"), IntNum(1)),
            Eq(Var("Z"), Div(Var("H2"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Div(IntNum(2), IntNum(1))),
//            Eq(Var(Name("H1")), IntNum(2)),
//            Eq(Var(Name("H2")), Div(IntNum(4), IntNum(2))),
            Eq(Var(Name("H3")), Div(IntNum(1), IntNum(2))),
            Eq(Var(Name("H4")), Div(Var("H1"), Var("H3"))),
//            Eq(Var(Name("H5")), Mul(Var("X"), Var("X"))),     // H1 / H3 = X / H3 = 2 / (1/2) = 2 * 2 = X * X
//            Eq(Var(Name("H6")), Div(Var("X"), IntNum(1))),
            Eq(Var(Name("H7")), Div(Add(Var("X"), IntNum(0)), IntNum(2))),
//            Eq(Var(Name("H8")), Add(Div(Var("X"), IntNum(2)), Div(IntNum(0), IntNum(2)))),
//            Eq(Var("H7"), Var("H7")),
//            Eq(Var("Y"), IntNum(1)),
//            Eq(Var("Z"), Div(Var("H2"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("H7"))),
            Eq(Var(Name("param$2")), Var(Name("H7")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Remainder/mod") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
            Eq(Var("C"), Remainder(IntNum(-16), IntNum(2))),
            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
            Eq(Var("E"), Mul(Var("A"),Mul(Var("B"), Mul(Var("C"), Var("D"))))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("E")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
//            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
//            Eq(Var("C"), Remainder(IntNum(-16), IntNum(2))),
//            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
//            Eq(Var("E"), Mul(Var("A"), Mul(Var("B"), Mul(Var("C"), Var("D"))))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Min") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("A"), Min(IntNum(16), Var("X"))),
            Eq(Var("B"), Min(IntNum(16), Var("Y"))),
            Eq(Var("C"), Min(Var("Y"), IntNum(16))),
            Eq(Var("D"), Min(Var("Y"), IntNum(100))),   //
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("D")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
            Eq(Var("A"), Min(IntNum(16), Var("X"))),
//            Eq(Var("B"), Min(IntNum(16), Var("Y"))),
//            Eq(Var("C"), Min(Var("Y"), IntNum(16))),
//            Eq(Var("D"), Min(Var("Y"), IntNum(100))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Max") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(5)),
            Eq(Var("A"), Max(IntNum(16), Var("X"))),
            Eq(Var("B"), Max(IntNum(16), Var("Y"))),
            Eq(Var("C"), Max(Var("Y"), IntNum(16))),
            Eq(Var("D"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("X")), Mul(IntNum(-1), IntNum(16))))),
            Eq(Var("E"), Max(Add(Var("Z"), Var("D")), Add(Var("Z"), IntNum(16)))),
            Eq(Var("F"), Add(Var("Z"), Max(Var("D"), IntNum(16)))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("F")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(5)),
            Eq(Var("A"), Max(IntNum(16), Var("X"))),
//            Eq(Var("B"), Max(IntNum(16), Var("Y"))),
//            Eq(Var("C"), Max(Var("Y"), IntNum(16))),
//            Eq(Var("D"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("X")), Mul(IntNum(-1), IntNum(16))))),
            Eq(Var("E"), Max(Add(Var("Z"), Var("A")), Add(Var("Z"), IntNum(16)))),
//            Eq(Var("F"), Add(Var("Z"), Max(Var("D"), IntNum(16)))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("E")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Abs") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(-2)),
            Eq(Var("A"), Abs(Var("X"))),
            Eq(Var("B"), Abs(Var("Y"))),
            Eq(Var("C"), Abs(Var("Z"))),
            Eq(Var("C"), Abs(Var("A"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("C")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(-2)),
//            Eq(Var("A"), Abs(Var("X"))),
//            Eq(Var("B"), Abs(Var("Y"))),
//            Eq(Var("C"), Abs(Var("Z"))),
//            Eq(Var("C"), Abs(Var("A"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Simpsons Example for Hash-Based"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),

            // from Simpson`s papers -> all equal 0 if X == Y
            Eq(Var("A"), Sub(Var("X"), Var("Y"))),
            Eq(Var("B"), Sub(Var("Y"), Var("X"))),
            Eq(Var("C"), Sub(Var("A"), Var("B"))),
            Eq(Var("D"), Sub(Var("B"), Var("A"))),

            Eq(Var(Name("param$0")), Var(Name("B"))),
            Eq(Var(Name("param$1")), Var(Name("C"))),
            Eq(Var(Name("param$2")), Var(Name("D")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), Var("X")),

            Eq(Var("A"), Sub(Var("X"), Var("X"))),
//            Eq(Var("B"), Var("A")),
//            Eq(Var("C"), Var("A")),
//            Eq(Var("D"), Var("A")),

            Eq(Var(Name("param$0")), Var("A")),
            Eq(Var(Name("param$1")), Var("A")),
            Eq(Var(Name("param$2")), Var("A"))
          ))
        ))
      ))
    performTest(expected, input)
  }

}
