package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.valueNumbering.{ConfigVN, ValueNumbering}


// tests with DoubleNum are in ArithmeticTest.scala

class BasicIRAndSimpleArithmeticTest extends ValueNumberingTestAbstract {


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
//            Eq(Var(Name("B")), Div(Mul(Var("X"), Var("X")), IntNum(2))), // should not be replaced with H1 from other body
            Eq(Var(Name("H1")), Div(Mul(Var("X"), Var("X")), IntNum(2))), // -> but they do compute the same value -> with global scope given same name
            Eq(Var(Name("Z")), Add(Var(Name("X")), Add(Var(Name("A")), Var(Name("H1"))))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant Eqs") { // currently one A == A remains <- the non binding Eqs are left by VN of terms but duplicates are removed by VN of atoms
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
            Eq(Var(Name("A")), Var("A")),   // can be removed too 
//            Eq(Var(Name("E")), IntNum(1)),
//            Eq(Var(Name("C")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant Eqs 2") { // see comments on prev test
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), IntNum(1)),
            Eq(Var(Name("B")), IntNum(1)),
            Eq(Var(Name("C")), Var("B")),
            Eq(Var(Name("D")), Var("A")),
            Eq(Var(Name("C")), Var("D")),
            Eq(Var(Name("E")), Mul(IntNum(1), IntNum(1))),  // remove even if Equality known later? But its actually already known here... Would that always be the case (with Calls etc)
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
            Eq(Var(Name("A")), Var("A")),
            //            Eq(Var(Name("C")), Var("E")),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected,input,ConfigVN(true))
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
//            LT(Var(Name("Z")), Var(Name("X"))),
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

  test("Redundant term in Eq with binding rhs var") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X1")), IntNum(1)),
            Eq(IntNum(1), Var(Name("X2"))),
            Eq(Var(Name("X2")), Var(Name("X3"))),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X3")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X1")), IntNum(1)),
//            Eq(IntNum(1), Var(Name("X2"))),
//            Eq(Var(Name("X2")), Var(Name("X3"))),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X1")))
          ))
        ))
      ))
    performTest(expected, input)
  }

//  test("comparison that should not be removed") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
//            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
//            Eq(Var(Name("Z")), Add(IntNum(2), IntNum(1))),
//            Eq(Var("Y"), Var("Z")),
//            Eq(Var(Name("param$0")), Var(Name("X"))),
//            Eq(Var(Name("param$1")), Var(Name("Y"))),
//            Eq(Var(Name("param$2")), Var(Name("Z")))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
//            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
////            Eq(Var(Name("Z")), Var(Name("X"))),
//            Eq(Var("Y"), Var("X")),     // this gets removed but should stay since it makes relation empty -> assumption that such bodies where treated before VN
//            Eq(Var(Name("param$0")), Var(Name("X"))),
//            Eq(Var(Name("param$1")), Var(Name("Y"))),
//            Eq(Var(Name("param$2")), Var(Name("X")))
//          ))
//        ))
//      ))
//    performTest(expected, input)
//  }

  test("If lowered (small)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("main"), Seq(Param("main_result$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("x")), IntNum(7)),
            Eq(Var(Name("if_result$0")), Var(Name("x"))),
            Eq(Var(Name("main_result$0")), Var(Name("if_result$0"))),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("main"), Seq(Param("main_result$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("x")), IntNum(7)),
            //            Eq(Var(Name("if_result$0")), Var(Name("x"))),
            Eq(Var(Name("main_result$0")), Var(Name("x"))),
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
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Eq(Var("X2"), Var("X1")),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X2")))
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
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Eq(Var("X2"), Var("X1")),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X1")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Non-Redundant Vars bound in Call") {
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
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X")))), false),
            Call(Name("b"), Seq(TermArg(Var(Name("Y")))), false),  // Cant be removed: is that correct if b has multiple bodies ???
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Non-Redundant Var bound in Call & replace Args") {
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
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(2))
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
            Call(Name("b"), Seq(TermArg(Var(Name("Y"))), TermArg(Var(Name("A")))), false),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(5))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(2))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Simple Redundant Call") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Eq(Var(Name("param$0")), Var(Name("X1")))
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
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            //Call(Name("b"), Seq(TermArg(Var(Name("X1"))), false),
            Eq(Var(Name("param$0")), Var(Name("X1")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          ))
        ))
      ))
    performTest(expected, input)
  }


  test("Redundant Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Eq(Var("X1"), Var("X2")),
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
            Eq(Var(Name("param$0")), Var(Name("X2")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Eq(Var("X1"), Var("X2")),
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
            Eq(Var(Name("param$0")), Var(Name("X1")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Ext Call replace Args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("C")), Var(Name("B"))),
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("A"))), TermArg(Var(Name("B"))), TermArg(Var(Name("C")))), false),
            ExtensionalCall(Name("b"), Seq(TermArg(IntNum(1)), TermArg(Var(Name("B"))), TermArg(Var(Name("B")))), false),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("C")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)))
      ))

    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), Add(IntNum(2), IntNum(3))),
            //            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            //            Eq(Var(Name("C")), Var(Name("B"))),
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("A"))), TermArg(Var(Name("A"))), TermArg(Var(Name("A")))), false),
            ExtensionalCall(Name("b"), Seq(TermArg(IntNum(1)), TermArg(Var(Name("A"))), TermArg(Var(Name("A")))), false),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)))
      ))
    performTest(expected, input)
  }

  test("Ext Call: Simple Redundant Var bound in Ext Call") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Eq(Var("X2"), Var("X1")),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X2")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            //            Eq(Var("X2"), Var("X1")),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X1")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    performTest(expected, input)
  }

  test("Redundant Ext Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            Eq(Var("X1"), Var("X2")),
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
            Eq(Var(Name("param$0")), Var(Name("X2")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            //            Eq(Var("X1"), Var("X2")),
            //            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
            //            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
            Eq(Var(Name("param$0")), Var(Name("X1")))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    performTest(expected, input)
  }

  test("Redundant Atoms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("X1"), IntNum(32)),
            Eq(Var("X1"), Var("X2")),
            Eq(Var("X2"), Var("X1")),
            GE(Var("X1"),IntNum(2)),
            GE(Var("X2"),IntNum(2)),
            LT(Var("X1"), IntNum(64)),
            LT(Var("X2"), IntNum(64)),
            Eq(Var("X1"), IntNum(4), true),
            Eq(Var("X2"), IntNum(4), true),
            Eq(Var(Name("param$0")), Var(Name("X2")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("X1"), IntNum(32)),
//            Eq(Var("X1"), Var("X2")),
            Eq(Var("X1"), Var("X1")),
            GE(Var("X1"), IntNum(2)),
//            GE(Var("X2"), IntNum(2)),
            LT(Var("X1"), IntNum(64)),
//            LT(Var("X2"), IntNum(64)),
            Eq(Var("X1"), IntNum(4), true),
//            Eq(Var("X2"), IntNum(4), true),
            Eq(Var(Name("param$0")), Var(Name("X1")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Redundant bodies") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"),IntNum(1)))
          )),
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n", Int(1))))
//          )),
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  // This does not work, since currently not known whether the order of atoms can be switched without changing the meaning of the program
  test("Redundant bodies 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          )),
          Body(Seq(
            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
//            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
//          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(4)),
            Eq(Var(Name("n")), IntNum(10)),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  test("Redundant Relation") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  test("Redundant Relation with renaming") { // TODO
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10)),
            GT(Var(Name("m")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("m"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10)),
            GT(Var(Name("param$0")), IntNum(0)),
            Eq(Var(Name("param$1")), Add(IntNum(1), Var("param$0")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(simplifyArithmetic = true, attemptAlphaEquivalence = true))
  }

  test("Redundant Relation with Call of removed Relation") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("result")),TermArg(Var("n"))))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("result")),TermArg(Var("n"))))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10)),
            GT(Var(Name("n")), IntNum(0)),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  // TODO how to let these tests pass and not break the 'Bus Station' Test ??? (for this commented out call of treatBindingInEq(...) in treatComparisonEq
  test("Call and check for Equality") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"),Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(IntNum(8),IntNum(2))),
            Eq(Var(Name("Y")), Add(IntNum(8),IntNum(2))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"),Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(IntNum(8),IntNum(2))),
            Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality with switched args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(IntNum(8), IntNum(2))),
            Eq(Add(IntNum(8), IntNum(2)), Var(Name("Y"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(IntNum(8), IntNum(2))),
            Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality with var bound in call used in term") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(Var("Y"), IntNum(2))), // now value of X also not known
            Eq(IntNum(12), Var(Name("Z"))),
            Eq(IntNum(12), Var(Name("X"))), // <- thus this var shouldn`t be replaced either
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(Var("Y"), IntNum(2))),
            Eq(Var(Name("Z")), IntNum(12)), // TODO could this be removed? -> would need to figure out order again or iterate(?)
            Eq(Var(Name("X")), IntNum(12)),
            Eq(Var(Name("param$0")), Var(Name("Z"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Repeated Atoms in different Relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("B1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2),Var("A1"))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  test("Repeated Atoms in different Relations 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("m", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(IntNum(2),Var("B1"))),
            Eq(Var(Name("m")), Mul(Var("B1"),Var("n"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1"))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("m", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2),Var("A1"))),
            Eq(Var(Name("m")), Mul(Var("A1"),Var("n"))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }


  test("Repeated Atoms in different Relations with Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("A1")))),
            Eq(Var(Name("A2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"),Var("A2"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("B1")))),
            Eq(Var(Name("B2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("B1"),Var("B2"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("n"), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("A1")))),
            Eq(Var(Name("A2")), IntNum(10)),
            Eq(Var(Name("n")), Mul(Var("A1"),Var("A2"))),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("n"), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

  test("Repeated Atoms in same Relation with Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Call(Name("c"), Seq(TermArg(Var("A3")))),
            Eq(Var(Name("A2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          )),
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("B1")))),
            Eq(Var(Name("B2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("B1"), Var("B2"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("n"), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Call(Name("c"), Seq(TermArg(Var("A3")))),
            Eq(Var(Name("A2")), IntNum(10)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))//,
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Eq(Var(Name("A2")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
//            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
//          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("n"), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true))
  }

}
