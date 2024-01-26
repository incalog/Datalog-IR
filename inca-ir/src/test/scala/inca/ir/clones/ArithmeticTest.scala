package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.analysis.{ConfigVN, ValueNumbering}


class ArithmeticTest extends ValueNumberingTestAbstract{

  override val config: ConfigVN = ConfigVN(true)


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
            Eq(Var(Name("H1")), Add(IntNum(2), Var("X"))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected, input)
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
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(IntNum(5), Var("X"))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected, input)
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
    performTest(expected, input)
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
    performTest(expected, input)
  }

  test("sub") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Sub(Var(Name("X")), Add(Var("X"), IntNum(4)))),
            Eq(Var(Name("H2")), IntNum(-4)),
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
            Eq(Var(Name("H1")), IntNum(-4)),
            //            Eq(Var(Name("H2")), IntNum(-4)),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("mul commutative") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(2)),
            Eq(Var(Name("H1")), Mul(IntNum(3), Var(Name("X")))),
            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(3))),
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
            Eq(Var(Name("H1")), Mul(IntNum(3), Var(Name("X")))),
            //            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(3))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected, input)
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
            Eq(Var(Name("H1")), Mul(IntNum(6), Var(Name("X")))),
            //Eq(Var(Name("H2")), Mul(Mul(IntNum(2), Var(Name("X"))), IntNum(3))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("mul & add distributivity") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("H1")), Mul(IntNum(2), Add(Var(Name("X")), IntNum(3)))),
            Eq(Var(Name("H2")), Add(Mul(IntNum(2), Var(Name("X"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("H3")), Mul(IntNum(2), Add(IntNum(4), IntNum(3)))),
            Eq(Var(Name("H4")), Mul(Var(Name("H1")), Add(IntNum(3), Var(Name("H3"))))),
            Eq(Var(Name("H5")), Add(Mul(Var(Name("H1")), Var(Name("H3"))), Mul(Var(Name("H1")), IntNum(3)))),
            Eq(Var(Name("Z")), Mul(Var(Name("H5")), Var(Name("H4")))),
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
            Eq(Var(Name("H1")), Add(IntNum(6), Mul(IntNum(2), Var(Name("X"))))),
            //Eq(Var(Name("H2")), Add(Mul(IntNum(2), Var(Name("X"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("H3")), IntNum(14)),
            Eq(Var(Name("H4")), Add(Mul(IntNum(3), Var(Name("H1"))), Mul(Var(Name("H1")), Var(Name("H3"))))),
            //            Eq(Var(Name("H5")), Add(Mul(Var(Name("H1")), Var(Name("H3"))), Mul(Var(Name("H1")), IntNum(3)))),
            Eq(Var(Name("Z")), Mul(Var(Name("H4")), Var(Name("H4")))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected, input)
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
            Eq(Var(Name("H1")), IntNum(0)),
            //            Eq(Var(Name("H2")), IntNum(0)),
            //            Eq(Var(Name("H3")), Mul(IntNum(0), Var(Name("X")))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))), // 0 + 0 -> 0 -> Var("H1") -> can be removed too
            //            Eq(Var(Name("Z")), Mul(Var("X"), IntNum(1))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected, input)
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
            //            Eq(Var("H9"), Div(IntNum(3), IntNum(2))),
            //            Eq(Var("H10"), Add(Var("H9"), IntNum(5))),
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
            //            Eq(Var(Name("H1")), IntNum(2)),
            //            Eq(Var(Name("H2")), Div(IntNum(4), IntNum(2))),
            Eq(Var(Name("H3")), IntNum(0)),
            Eq(Var(Name("H4")), Div(Var("X"), Var("H3"))),
            Eq(Var(Name("H5")), Mul(Var("X"), Var("X"))), // if H3 would not result of integer division then this would be redundant too
            //            Eq(Var(Name("H6")), Div(Var("X"), IntNum(1))),
            Eq(Var(Name("H7")), Div(Var("X"), IntNum(2))),
            //            Eq(Var(Name("H8")), Add(Div(Var("X"), IntNum(2)), Div(IntNum(0), IntNum(2)))),
            //            Eq(Var("H7"), Var("H7")),
            Eq(Var("Y"), IntNum(1)),
            //            Eq(Var("Z"), Div(Var("H2"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Y")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Remainder/mod") { // TODO cases that would not be solved by constant propagation ?
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(4)),
            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
            Eq(Var("B2"), Remainder(Add(IntNum(6), IntNum(10)), Var("Y"))),
            Eq(Var("C"), Remainder(IntNum(-16), IntNum(2))),
            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
            Eq(Var("E"), Mul(Var("A"), Mul(Var("B"), Mul(Var("C"), Var("D"))))),
            Eq(Var("F"), Remainder(Mul(Var("Z"), IntNum(4)), IntNum(2))),
            Eq(Var("G"), Remainder(Mul(Var("X"), IntNum(4)), IntNum(2))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("E")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            //            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(4)),
            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
            //            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
            Eq(Var("C"), IntNum(0)),
            //            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
            Eq(Var("E"), Mul(Var("A"), Mul(Var("A"), Mul(Var("C"), Var("C"))))),
            Eq(Var("F"), Remainder(Mul(IntNum(4), Var("Z")), IntNum(2))),
            Eq(Var("G"), Remainder(Mul(IntNum(4), Var("X")), IntNum(2))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("E")))
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
            //            Eq(Var("D"), Min(Var("Y"), IntNum(100))),
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
            Eq(Var("Z"), IntNum(5)),
            Eq(Var("A"), Mul(IntNum(-1), Min(IntNum(-16), Mul(IntNum(-1), Var("X"))))),
            //            Eq(Var("B"), Max(IntNum(16), Var("Y"))),
            //            Eq(Var("C"), Max(Var("Y"), IntNum(16))),
            //            Eq(Var("D"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("X")), Mul(IntNum(-1), IntNum(16))))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("A")))
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
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
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
            Eq(Var("A"), Abs(Var("X"))),
            //            Eq(Var("B"), Abs(Var("Y"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Simpsons Example for Hash-Based") {
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

            Eq(Var("A"), IntNum(0)),
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

  test("propagate constants") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            Eq(Var(Name("H2")), Add(IntNum(2), Var("X"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))),
            GT(Var(Name("Z")),Div(Var("Y"),IntNum(3))),
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
//            Eq(Var(Name("X")), IntNum(1)),
//            Eq(Var(Name("Y")), IntNum(3)),
//            Eq(Var(Name("H1")), Add(IntNum(2), Var("X"))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
//            Eq(Var(Name("Z")), IntNum(6)),
            GT(IntNum(6),IntNum(1)),    // TODO can be concluded that true -> could be removed
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(6))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: zero") {
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
//            Eq(Var(Name("X")), IntNum(2)),
//            Eq(Var(Name("H1")), IntNum(0)),
            //            Eq(Var(Name("H2")), IntNum(0)),
            //            Eq(Var(Name("H3")), Mul(IntNum(0), Var(Name("X")))),
//            Eq(Var(Name("Y")), Add(Var("H1"), Var("H1"))), // 0 + 0 -> 0 -> Var("H1") -> can be removed too
            //            Eq(Var(Name("Z")), Mul(Var("X"), IntNum(1))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(0)),
            Eq(Var(Name("param$2")), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: Remainder/mod") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("Z"), IntNum(4)),
            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
            Eq(Var("B2"), Remainder(Add(IntNum(6), IntNum(10)), Var("Y"))),
            Eq(Var("C"), Remainder(IntNum(-16), IntNum(2))),
            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
            Eq(Var("E"), Mul(Var("A"), Mul(Var("B"), Mul(Var("C"), Var("D"))))),
            Eq(Var("F"), Remainder(Mul(Var("Z"), IntNum(4)), IntNum(2))),
            Eq(Var("G"), Remainder(Mul(Var("X"), IntNum(4)), IntNum(2))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B"))),
            Eq(Var(Name("param$2")), Var(Name("E")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
//            Eq(Var("X"), IntNum(2)),
            //            Eq(Var("Y"), IntNum(2)),
//            Eq(Var("Z"), IntNum(4)),
//            Eq(Var("A"), Remainder(IntNum(16), Var("X"))),
            //            Eq(Var("B"), Remainder(IntNum(16), Var("Y"))),
//            Eq(Var("C"), IntNum(0)),
            //            Eq(Var("D"), Remainder(IntNum(2), IntNum(2))),
//            Eq(Var("E"), Mul(Var("A"), Mul(Var("A"), Mul(Var("C"), Var("C"))))),
//            Eq(Var("F"), Remainder(Mul(IntNum(4), Var("Z")), IntNum(2))),
//            Eq(Var("G"), Remainder(Mul(IntNum(4), Var("X")), IntNum(2))),
            Eq(Var(Name("param$0")), IntNum(0)),
            Eq(Var(Name("param$1")), IntNum(0)),
            Eq(Var(Name("param$2")), IntNum(0))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: Min") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), IntNum(2)),
            Eq(Var("Y"), IntNum(2)),
            Eq(Var("A"), Min(IntNum(16), Var("X"))),
            Eq(Var("B"), Min(IntNum(16), Var("Y"))),
            Eq(Var("C"), Min(Var("Y"), IntNum(16))),
            Eq(Var("D"), Min(Var("Y"), IntNum(100))),
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
//            Eq(Var("X"), IntNum(2)),
            //            Eq(Var("Y"), IntNum(2)),
//            Eq(Var("A"), Min(IntNum(16), Var("X"))),
            //            Eq(Var("B"), Min(IntNum(16), Var("Y"))),
            //            Eq(Var("C"), Min(Var("Y"), IntNum(16))),
            //            Eq(Var("D"), Min(Var("Y"), IntNum(100))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(2)),
            Eq(Var(Name("param$2")), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: Max") {
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
            Eq(Var(Name("param$2")), Var(Name("E")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
//            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
//            Eq(Var("Z"), IntNum(5)),
//            Eq(Var("A"), Mul(IntNum(-1), Min(IntNum(-16), Mul(IntNum(-1), Var("X"))))),
//            Eq(Var("B"), Max(IntNum(16), Var("Y"))),
//            Eq(Var("C"), Max(Var("Y"), IntNum(16))),
//            Eq(Var("D"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("X")), Mul(IntNum(-1), IntNum(16))))),
//            Eq(Var("E"), Max(Add(Var("Z"), Var("D")), Add(Var("Z"), IntNum(16)))),
//            Eq(Var("F"), Add(Var("Z"), Max(Var("D"), IntNum(16)))),
            Eq(Var(Name("param$0")), IntNum(16)),
            Eq(Var(Name("param$1")), IntNum(16)),
            Eq(Var(Name("param$2")), IntNum(21))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: Abs") {
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
//            Eq(Abs(Var("C")), Var("D")),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
//            Eq(Var("X"), IntNum(2)),
//            Eq(Var("Y"), IntNum(2)),
//            Eq(Var("Z"), IntNum(-2)),
//            Eq(Var("A"), Abs(Var("X"))),
//            Eq(Var("B"), Abs(Var("Y"))),
//            Eq(Var("C"), Abs(Var("Z"))),
//            Eq(Var("C"), Abs(Var("A"))),
//            Eq(Abs(Var("A")), Var("D")),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(2)),
            Eq(Var(Name("param$2")), IntNum(-2))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("propagate constants: Two Bodies") {
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
            Eq(Var(Name("B")), Div(Mul(Var("Y"), Var("X")), IntNum(2))), // should not be replaced with H1 from other body
            Eq(Var(Name("Z")), Add(Var(Name("X")), Add(Var(Name("A")), Var(Name("B"))))),
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
//            Eq(Var(Name("X")), IntNum(5)),
            //Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("H1")), IntNum(12)),
            //Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
//            Eq(Var(Name("Z")), IntNum(-24)),
            Eq(Var(Name("param$0")), IntNum(5)),
            Eq(Var(Name("param$1")), IntNum(5)),
            Eq(Var(Name("param$2")), IntNum(-24))
          )),
          Body(Seq(
//            Eq(Var(Name("X")), IntNum(4)),
            //Eq(Var(Name("Y")), Mul(IntNum(2), IntNum(2))),
//            Eq(Var(Name("A")), IntNum(1)),
//            Eq(Var(Name("B")), IntNum(8)), // should not be replaced with H1 from other body
//            Eq(Var(Name("Z")), IntNum(13)),
            Eq(Var(Name("param$0")), IntNum(4)),
            Eq(Var(Name("param$1")), IntNum(4)),
            Eq(Var(Name("param$2")), IntNum(13))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true,true))
  }

  test("propagate constants: Call replace Args") {
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
//            Eq(Var(Name("A")), IntNum(5)),
            //            Eq(Var(Name("B")), Add(IntNum(2), IntNum(3))),
            //            Eq(Var(Name("C")), Var(Name("B"))),
            Call(Name("b"), Seq(TermArg(IntNum(5)), TermArg(IntNum(5)), TermArg(IntNum(5))), false),
            Call(Name("b"), Seq(TermArg(IntNum(1)), TermArg(IntNum(5)), TermArg(IntNum(5))), false),
            Eq(Var(Name("param$0")), IntNum(5)),
            Eq(Var(Name("param$1")), IntNum(5))
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
    performTest(expected, input, ConfigVN(true,true))
  }

  test("Redundant Term in Eq: multiple relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(IntNum(2), Var("X1")),
            Eq(Var(Name("param$0")), Var(Name("X1")))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), Add(IntNum(1), IntNum(10)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(true, true))
  }

  test("Redundant term in Eq with more Eqs with same Var") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(1)),
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(1))),
            Eq(Var(Name("Z1")), Div(IntNum(1), IntNum(1))),
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
//            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(1))), // TODO gets remembered correctly but could be removed here
            //            Eq(Var(Name("Z1")), Mul(IntNum(1), IntNum(1)))),
            //            Eq(Var(Name("Z2")), IntNum(1)),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected, input)
  }

}
