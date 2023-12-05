package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.analysis.ValueNumbering


class ClonesTest extends AnyFunSuite {

  test("Simple Redundant Expr in Eq") {
    val VN = new ValueNumbering
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq with Add"){
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(2)),
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
            Eq(Var(Name("Y")), IntNum(2)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq with Sub") {
    val VN = new ValueNumbering
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq with Mul and Div") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(1), IntNum(2))),
            Eq(Var(Name("Y")), Mul(IntNum(1), IntNum(2))),
            Eq(Var(Name("H1")), Div(Var("Y"), IntNum(2))),
            Eq(Var(Name("H2")), Div(Var("X"), IntNum(2))),
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
            Eq(Var(Name("H1")), Div(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Div(Var("X"), IntNum(2))),
            Eq(Var(Name("Z")), Add(Var("H1"), Mul(Var("H1"), IntNum(3)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq nested") {
    val VN = new ValueNumbering
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
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq: Two Bodies") {
    val VN = new ValueNumbering
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in Eq with Gt, Lt & Neq") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(1)),
            LT(Var(Name("H1")), Add(Var(Name("Y")),IntNum(1))), // TODO would var be bound before?
            GT(Var(Name("H2")), Add(Var(Name("Y")),IntNum(1))),
            Eq(Var(Name("Y")), Var(Name("H1")), true),
            LT(Var(Name("Y")), Add(Var(Name("X")),IntNum(1))),
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
            LT(Var(Name("H1")), Add(Var(Name("X")),IntNum(1))),
            GT(Var(Name("H2")), Add(Var(Name("X")),IntNum(1))),
            Eq(Var(Name("X")), Var(Name("H1")), true),
            LT(Var(Name("X")), Add(Var(Name("X")),IntNum(1))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr in LT") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Y")), Add(IntNum(2), IntNum(1))),
            LT(Var(Name("Z")), Add(IntNum(2), IntNum(1))),  // TODO replace this too?
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
            LT(Var(Name("Z")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Simple Redundant Expr bound in Call") {
    val VN = new ValueNumbering
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Redundant Expr bound in Call & replace Args") {
    val VN = new ValueNumbering
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
            Eq(Var(Name("param$0")), IntNum(5))
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Add (Commutativity)") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(2)),
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
            Eq(Var(Name("Y")), IntNum(2)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Add (Associativity)") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(2)),
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("Add zero") {
    val VN = new ValueNumbering
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
    assertResult(expected)(VN.valueNumbering(input))
  }

  test("sub not commutative") {
    val VN = new ValueNumbering
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
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
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("H1")), Sub(IntNum(1), Var(Name("X")))),
            Eq(Var(Name("H2")), Sub(Var(Name("X")), IntNum(1))),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    assertResult(expected)(VN.valueNumbering(input))
  }

}
