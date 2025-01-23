package inca.ir.valueNumbering.extensions

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.extension.impure.MainHint
import inca.ir.valueNumbering.ValueNumberingTestAbstract


class ArithmeticTest extends ValueNumberingTestAbstract{

  test("Add (Commutativity)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))), // -> H1 == Y
            Eq(Var(Name("H2")), Add(IntNum(2), Var("X"))), // -> H2 == H1 (commutativity)
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
//            Eq(Var(Name("X")), IntNum(1)),
//            Eq(Var(Name("Y")), IntNum(3)),
//            Eq(Var(Name("H1")), Add(IntNum(2), Var("X"))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
//            Eq(Var(Name("Z")), Mul(IntNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(6))
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
//            Eq(Var(Name("X")), IntNum(1)),
//            Eq(Var(Name("Y")), IntNum(3)),
//            Eq(Var(Name("H1")), IntNum(6)),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
//            Eq(Var(Name("Z")), IntNum(12)),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(12))
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
//            Eq(Var(Name("X")), IntNum(1)),
            //Eq(Var(Name("H1")), Var(Name("X"))),
            //Eq(Var(Name("H2")), Var(Name("X"))),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("X"))),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Add nested multiple times") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Add(Var("X"), Add(IntNum(2), Add(IntNum(3), Var("Y"))))),
            Eq(Var(Name("H2")), Add(Add(Var("X"), Add(IntNum(2), IntNum(3))), Var("Y"))),
            Eq(Var(Name("H3")), Add(Add(Var("X"), Add(IntNum(2), IntNum(3))), Add(Var("Y"), IntNum(0)))),
            Eq(Var(Name("H2")), Add(Add(Var("X"), Add(IntNum(-2), IntNum(7))), Var("Y"))),
            Eq(Var(Name("H4")), Add(Add(Var("Y"), Add(IntNum(2), IntNum(3))), Var("X"))),
            Eq(Var(Name("H5")), Add(Add(Var("X"), Var("Y")), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H5")), Add(Var("X"), Add(Var("Y"), Add(IntNum(2), IntNum(3))))),
            Eq(Var(Name("H6")), Add(IntNum(2), Add(Var("Y"), Add(IntNum(1), Add(Var("X"), IntNum(2)))))),
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
//            Eq(Var(Name("X")), IntNum(1)),
//            Eq(Var(Name("Y")), IntNum(3)),
//            Eq(Var(Name("H1")), Add(IntNum(5), Add(Var("X"), Var("Y")))),
//            Eq(IntNum(9), IntNum(9)),
//            Eq(IntNum(9), IntNum(9)),
//            Eq(Var(Name("Z")), Mul(IntNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(18))
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
            Eq(Var(Name("H1")), Var(Name("H2")),true),
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
//            Eq(Var(Name("X")), IntNum(2)),
//            Eq(Var(Name("H1")), Sub(IntNum(1), Var(Name("X")))),
//            Eq(Var(Name("H2")), Sub(Var(Name("X")), IntNum(1))),
//            Eq(IntNum(-1), IntNum(1), true),
//            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(0))
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
//            Eq(Var(Name("X")), IntNum(2)),
//            Eq(Var(Name("H1")), IntNum(-4)),
            //            Eq(Var(Name("H2")), IntNum(-4)),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(-8))
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
//            Eq(Var(Name("X")), IntNum(2)),
//            Eq(Var(Name("H1")), Mul(IntNum(3), Var(Name("X")))),
            //            Eq(Var(Name("H2")), Mul(Var(Name("X")), IntNum(3))),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(12))
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
//            Eq(Var(Name("X")), IntNum(3)),
//            Eq(Var(Name("H1")), Mul(IntNum(6), Var(Name("X")))),
            //Eq(Var(Name("H2")), Mul(Mul(IntNum(2), Var(Name("X"))), IntNum(3))),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(3)),
            Eq(Var(Name("param$1")), IntNum(36))
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
//            Eq(Var(Name("X")), IntNum(1)),
//            Eq(Var(Name("H1")), Add(IntNum(6), Mul(IntNum(2), Var(Name("X"))))),
            //Eq(Var(Name("H2")), Add(Mul(IntNum(2), Var(Name("X"))), Mul(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("H1"))),
//            Eq(Var(Name("H3")), IntNum(14)),
//            Eq(Var(Name("H4")), Add(Mul(Var(Name("H1")), Var(Name("H3"))),Mul(IntNum(3), Var(Name("H1"))))),
            //            Eq(Var(Name("H5")), Add(Mul(Var(Name("H1")), Var(Name("H3"))), Mul(Var(Name("H1")), IntNum(3)))),
//            Eq(Var(Name("Z")), Mul(Var(Name("H4")), Var(Name("H4")))),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(16)),
            Eq(Var(Name("param$2")), IntNum(136*136))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Replacement of sub-term in call") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(121)),
            Call("S", Seq(TermArg(Add(Var("b"), IntNum(2))))),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(0))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(IntNum(123)))),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(0))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: mul & add distributivity") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("X"), Mul(Add(Var("A"), IntNum(1)), Add(IntNum(3),Var("A")))),
            Eq(Var("X2"), Add(Add(IntNum(3),Mul(IntNum(3), Var("A"))), Add(Var("A"), Mul(Var("A"),Var("A"))))), // ((3 + (3 * A: <TInt>)) + (A: <TInt> + (A: <TInt> * A: <TInt>)))
            Eq(Var("X3"), Add(IntNum(3), Add(Var("A"), Add(Mul(IntNum(3),Var("A")), Mul(Var("A"),Var("A"))))))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("X"), Add(IntNum(3), Add(Var("A"), Add(Mul(IntNum(3),Var("A")), Mul(Var("A"),Var("A"))))))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
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
//            Eq(Var(Name("X")), IntNum(2)),
//            Eq(Var(Name("H1")), IntNum(0)),
            //            Eq(Var(Name("H2")), IntNum(0)),
            //            Eq(Var(Name("H3")), Mul(IntNum(0), Var(Name("X")))),
//            Eq(Var(Name("Y")), Mul(IntNum(2), Var("H1"))),
            //            Eq(Var(Name("Z")), Mul(Var("X"), IntNum(1))),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(0)),
            Eq(Var(Name("param$2")), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Mul nested multiple times") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(3)),
            Eq(Var(Name("H1")), Mul(Var("X"), Mul(IntNum(2), Mul(IntNum(3), Var("Y"))))),
            Eq(Var(Name("H2")), Mul(Mul(Var("X"), Mul(IntNum(2), IntNum(3))), Var("Y"))),
            Eq(Var(Name("H3")), Mul(Mul(Var("X"), Mul(IntNum(2), IntNum(3))), Mul(Var("Y"), IntNum(1)))),
            Eq(Var(Name("H2")), Mul(Mul(Var("X"), Mul(IntNum(-2), IntNum(-3))), Var("Y"))),
            Eq(Var(Name("H4")), Mul(Mul(Var("Y"), Mul(IntNum(2), IntNum(3))), Var("X"))),
            Eq(Var(Name("H5")), Mul(Mul(Var("X"), Var("Y")), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("H5")), Mul(Var("X"), Mul(Var("Y"), Mul(IntNum(2), IntNum(3))))),
            Eq(Var(Name("H6")), Mul(IntNum(3), Mul(Var("Y"), Mul(IntNum(1), Mul(Var("X"), IntNum(2)))))),
            Eq(Var(Name("Z")), Mul(Var("H1"), Var("H2"))),
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
//            Eq(Var(Name("H1")), Mul(IntNum(6), Mul(Var("X"), Var("Y")))),
//            Eq(Var(Name("H2")), Mul(Mul(Var("X"), Mul(IntNum(2), IntNum(3))), Var("Y"))),
//            Eq(Var(Name("H3")), Mul(Mul(Var("X"), Mul(IntNum(2), IntNum(3))), Mul(Var("Y"), IntNum(1)))),
//            Eq(IntNum(18), IntNum(18)),
//            Eq(Var(Name("H4")), Mul(Mul(Var("Y"), Mul(IntNum(2), IntNum(3))), Var("X"))),
//            Eq(Var(Name("H5")), Mul(Mul(Var("X"), Var("Y")), Mul(IntNum(2), IntNum(3)))),
//            Eq(IntNum(18), IntNum(18)),
//            Eq(Var(Name("H6")), Mul(IntNum(3), Mul(Var("Y"), Mul(IntNum(1), Mul(Var("X"), IntNum(2)))))),
//            Eq(Var(Name("Z")), Mul(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(18*18))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("mul and add") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X1")), IntNum(2)),
            Eq(Var("X2"), Mul(Var("X1"), IntNum(2))),
            Eq(Var("X3"), Add(Var("X1"), Var("X1"))),
            Eq(Var("X4"), Mul(Var("X1"), IntNum(3))),
            Eq(Var("X5"), Add(Var("X1"), Add(Var("X1"),Var("X1")))),
            Eq(Var("X5"), Add(Add(Var("X1"),Var("X1")),Var("X1"))),
            Eq(Var("X6"), Mul(Var("X1"), IntNum(4))),
            Eq(Var("X7"), Add(Var("X1"), Add(Var("X1"),Add(Var("X1"),Var("X1"))))),
            Eq(Var("X7"), Add(Add(Var("X1"),Add(Var("X1"),Var("X1"))), Var("X1"))),
            Eq(Var(Name("param$0")), Var(Name("X1"))),
            Eq(Var(Name("param$1")), Var(Name("X2"))),
            Eq(Var(Name("param$2")), Var(Name("X5")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
//            Eq(Var(Name("X1")), IntNum(2)),
//            Eq(Var("X2"), Mul(IntNum(2),Var("X1"))),
//            Eq(Var("X3"), Add(Var("X1"), Var("X1"))),
//            Eq(Var("X4"), Mul(IntNum(3),Var("X1"))),
//            Eq(Var("X5"), Add(Var("X1"), Add(Var("X1"),Var("X1")))),
//            Eq(IntNum(6), IntNum(6)),
//            Eq(Var("X6"), Mul(IntNum(4),Var("X1"))),
//            Eq(Var("X7"), Add(Var("X1"), Add(Var("X1"),Add(Var("X1"),Var("X"))))),
//            Eq(IntNum(8), IntNum(8)),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(4)),
            Eq(Var(Name("param$2")), IntNum(6))
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
            Eq(Var(Name("H3")), Div(IntNum(6), IntNum(2))),
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
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(1)),
            Eq(Var(Name("param$2")), IntNum(1))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("div and mul") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), Div(IntNum(2), IntNum(1))),
            Eq(Var("temp"), Mul(Var("n"),Div(IntNum(1),Var("n")))),
            Eq(Var("temp2"), Mul(Var("temp"),Div(IntNum(1),Var("n")))),
            Eq(Var("temp3"), Mul(Div(IntNum(3),Var("n")),Var("n"))),
            Eq(Var("temp4"), Div(Var("n"),Mul(IntNum(3),Var("n")))),
            Eq(Var(Name("result")), Var(Name("temp")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(2)),
//            Eq(Var("temp"), IntNum(1)),
//            Eq(Var("temp2"), Mul(Var("temp"),Div(IntNum(1),Var("temp")))),
//            Eq(Var("temp3"), IntNum(3)),
//            Eq(Var("temp4"), IntNum(0)),
            Eq(Var(Name("result")), IntNum(0))
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
//            Eq(IntNum(2), IntNum(2)),
            Eq(Var(Name("param$0")), IntNum(2)),
            Eq(Var(Name("param$1")), IntNum(2)),
            Eq(Var(Name("param$2")), IntNum(-2))
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
//            Eq(Var("X"), IntNum(2)),
            //            Eq(Var("Y"), Var("X")),

//            Eq(Var("A"), IntNum(0)),
            //            Eq(Var("B"), Var("A")),
            //            Eq(Var("C"), Var("A")),
            //            Eq(Var("D"), Var("A")),

            Eq(Var(Name("param$0")), IntNum(0)),
            Eq(Var(Name("param$1")), IntNum(0)),
            Eq(Var(Name("param$2")), IntNum(0))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("various operations and GT") {
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
//            GT(IntNum(6),IntNum(1)),    // concluded that true -> removed
            Eq(Var(Name("param$0")), IntNum(1)),
            Eq(Var(Name("param$1")), IntNum(3)),
            Eq(Var(Name("param$2")), IntNum(6))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("various operations with zero") {
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
    performTest(expected, input)
  }

  test("Two Bodies") {
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
    performTest(expected, input)
  }

  test("Multiple Relations") {
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
    performTest(expected, input)
  }

  test("params bound in calls with arithmetic laws") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))),
            Eq(Var(Name("H1")), Add(Var("a"), IntNum(2))),
            Eq(Var(Name("H2")), Add(IntNum(2), Var("a"))),
            Eq(Add(Var("H1"), Var("H2")), Var(Name("H3"))),
            Call(Name("S"), Seq(TermArg(Var("b")))),
            Eq(Var(Name("H4")), Mul(IntNum(2), Add(Var(Name("H3")), IntNum(3)))),
            Eq(Var(Name("H5")), Add(Mul(IntNum(2), Var(Name("H3"))), Mul(IntNum(2), IntNum(3)))), // 2nd: same as above
            Eq(Var(Name("result")), Var(Name("H5")))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))),
            Eq(Var(Name("H1")), Add(IntNum(2), Var("a"))),
//            Eq(Var(Name("H2")), Add(IntNum(2), Var("a"))),
            Eq(Var(Name("H3")), Mul(IntNum(2), Var("H1"))),
            Call(Name("S"), Seq(TermArg(Var("b")))),
            Eq(Var(Name("result")), Add(IntNum(6), Mul(IntNum(2), Var(Name("H3"))))),
//            Eq(Var(Name("H5")), Add(Mul(IntNum(2), Var(Name("H3"))), Mul(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("result")), Var(Name("result")))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("params bound in calls with arithmetic laws (with Sub)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))),
            Eq(Var(Name("H1")), Sub(Var("a"), Add(IntNum(2),Var("a")))),
            Eq(Var(Name("H2")), Sub(Add(IntNum(-2), Var("a")),Var("a"))),
            Eq(Add(Var("H1"), Var("H2")), Var(Name("H3"))),
            Call(Name("S"), Seq(TermArg(Var("b")))),
            Eq(Var(Name("H4")), Mul(IntNum(2), Sub(Var(Name("H3")), IntNum(3)))),
            Eq(Var(Name("H5")), Sub(Mul(IntNum(2), Var(Name("H3"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("result")), Var(Name("H5")))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))),
//            Eq(Var(Name("H1")), IntNum(-2)),
//            Eq(Var(Name("H2")), Sub(Add(IntNum(-2), Var("a")),Var("a"))),
//            Eq(Var(Name("H3")),Mul(IntNum(2), Var("H1"))),
            Call(Name("S"), Seq(TermArg(Var("b")))),
//            Eq(Var(Name("H4")), Sub(Mul(IntNum(2),Var(Name("H3"))), IntNum(6))),
//            Eq(Var(Name("H5")), Sub(Mul(IntNum(2), Var(Name("H3"))), Mul(IntNum(2), IntNum(3)))),
            Eq(Var(Name("result")), IntNum(-14))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("param bound in call with arithmetic laws (with Comparison EQs)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))), // 2nd: S(5)
            Eq(Var("a"), Var("c")),
            Eq(Var("H6"), Sub(Var("a"), Var("c"))),
            Eq(Var("c"), IntNum(5)), // -> a == 5;  not 5 == 5 in 2nd because a is a param
            Eq(Var("H7"), Sub(IntNum(5), Var("c"))),
            Eq(Var("H6"), Var("H7")), // H6 == H7 -> 0 == 0
            Eq(Var(Name("result")), Var(Name("H7")))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("a")))),
            //            Eq(Var("a"),Var("c")),
//            Eq(Var("H6"), IntNum(0)),
            Eq(Var("a"), IntNum(5)),
            //            Eq(Var("H7"),IntNum(0)),
//            Eq(IntNum(0),IntNum(0)),
            Eq(Var(Name("result")), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Add"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var(Name("H1")), Add(Var("b"), IntNum(2))),
            Eq(Var(Name("H2")), Add(IntNum(2), Var("b"))),
            Eq(Var("H3"), Add(IntNum(0),Var("c"))),
            Eq(Var("H4"), Add(Var("c"),Var("c"))),
            Eq(Var("H5"), Mul(IntNum(2),Var("c"))),
            Eq(Var("H6"), Add(Add(IntNum(2),Var("c")),Var("b"))),
            Eq(Var("H7"), Add(IntNum(0),Add(Var("c"),Add(Var("b"),IntNum(2))))),
            Eq(Var("H8"), Add(Var("c"),Add(IntNum(2),Var("b")))),
            Eq(Var("H9"), Add(Add(Var("a"),Var("b")),Var("c"))),
            Eq(Var("H10"), Add(Var("a"), Add(Var("b"),Var("c")))),
            Eq(Add(Var("H1"), Var("H2")), Var(Name("result"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var(Name("H1")), Add(IntNum(2), Var("b"))),
//            Eq(Var(Name("H2")), Add(IntNum(2), Var("b"))),
//            Eq(Var("H3"), Add(IntNum(0),Var("c"))),
            Eq(Var("H4"), Mul(IntNum(2),Var("c"))),
//            Eq(Var("H5"), Mul(IntNum(2),Var("c"))),
            Eq(Var("H6"), Add(IntNum(2), Add(Var("b"),Var("c")))),
//            Eq(Var("H7"), Add(IntNum(0),Add(Var("c"),Add(Var("b"),IntNum(2))))),
//            Eq(Var("H8"), Add(Var("c"),Add(IntNum(2),Var("b")))),
            Eq(Var("H9"), Add(Var("a"), Add(Var("b"),Var("c")))),
//            Eq(Var("H10"), Add(Var("a"), Add(Var("b"),Var("c")))),
            Eq(Var(Name("result")), Mul(IntNum(2), Var("H1"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Mul") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
//            Eq(Var("H1"), Mul(Var("a"), IntNum(0))),
//            Eq(Var("H2"), Mul(IntNum(0),Var("a"))),
//            Eq(Var("H3"), Mul(IntNum(1),Var("a"))),
//            Eq(Var("H4"), Mul(Var("a"),Var("b"))),
//            Eq(Var("H5"), Mul(Var("b"),Var("a"))),
            Eq(Var("H6"), Mul(Var("a"), Mul(Var("b"),Var("c")))),
            Eq(Var("H7"), Mul(Var("c"), Mul(Var("b"),Var("a")))),
            Eq(Var("H8"), Mul(Var("b"), Mul(Var("c"),Var("a")))),
            Eq(Var("H9"), Mul(Mul(Var("b"),Var("c")),Var("a"))),
            Eq(Var("H10"), Mul(Mul(Var("c"),Var("a")),Var("b"))),
            Eq(Var("H11"), Mul(Mul(Var("a"),Var("b")),Var("c"))),
            Eq(Sub(Var("H6"), Var("H8")), Var(Name("result")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
//            Eq(Var("H1"), IntNum(0)),
//            Eq(Var("H2"), Mul(IntNum(0),Var("a"))),
//            Eq(Var("H3"), Var("a")),
//            Eq(Var("H4"), Mul(Var("a"),Var("b"))),
//            Eq(Var("H5"), Mul(Var("b"),Var("a"))),
            Eq(Var("H6"), Mul(Var("a"), Mul(Var("b"),Var("c")))),
//            Eq(Var("H7"), Mul(Var("c"), Mul(Var("b"),Var("a")))),
//            Eq(Var("H8"), Mul(Var("b"), Mul(Var("c"),Var("a")))),
//            Eq(Var("H9"), Mul(Mul(Var("b"),Var("c")),Var("a"))),
//            Eq(Var("H10"), Mul(Mul(Var("c"),Var("a")),Var("b"))),
//            Eq(Var("H11"), Mul(Mul(Var("a"),Var("b")),Var("c"))),
            Eq(Var("result"),IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Sub") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var(Name("H1")), Sub(Var("b"), IntNum(2))),
            Eq(Var(Name("H2")), Sub(IntNum(2), Var("b"))),
            Eq(Var("H3"), Sub(IntNum(0), Var("c"))),
            Eq(Var("H4"), Mul(IntNum(-1), Var("c"))),
            Eq(Var("H5"), Add(IntNum(0), Mul(IntNum(-1), Var("c")))),
            Eq(Var("a"),Var("b")),
            Eq(Var("H6"), Sub(Add(Var("a"), Var("b")), Add(Var("b"), Var("a")))),
            Eq(Var("H7"), Sub(Var(Name("c")), Add(Var("c"), IntNum(4)))),
            Eq(Var("H8"), Sub(Add(Var("c"), IntNum(-4)),Var(Name("c")))),
            Eq(Add(Var("H1"), Var("H2")), Var(Name("result"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var(Name("H1")), Add(IntNum(-2), Var("b"))),
            Eq(Var(Name("H2")), Add(IntNum(2), Mul(IntNum(-1),Var("b")))),
            Eq(Var("H3"), Mul(IntNum(-1), Var("c"))),
//            Eq(Var("H4"), Mul(IntNum(-1), Var("c"))),
//            Eq(Var("H5"), Add(IntNum(0), Mul(IntNum(-1), Var("c")))),
//            Eq(Var("b"),Var("b")),
//            Eq(Var("H6"), IntNum(0)),
//            Eq(Var("H7"), IntNum(-4)),
//            Eq(Var("H8"), IntNum(-4)),
            Eq(Var("result"),Add(Var("H1"), Var("H2"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Remainder") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var("a"),Var("b")),
            Eq(Var("H1"), Remainder(Var("a"),IntNum(2))),
            Eq(Var("H2"), Remainder(Var("b"),IntNum(2))),
            Eq(Var("H3"), Remainder(Var("a"),Var("c"))),
            Eq(Var("H4"), Remainder(Mul(Var("c"),IntNum(2)),IntNum(2))),
            Eq(Var("H5"), Remainder(Var("c"),IntNum(2))),
            Eq(Var("H6"), Remainder(Var("c"),Var("c"))),
            Eq(Var("H7"), Remainder(Var("a"),Var("b"))),
            Eq(Remainder(Var("H6"),Var("H1")), Var(Name("result"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
//            Eq(Var("b"),Var("b")),
            Eq(Var("H1"), Remainder(Var("b"),IntNum(2))),
//            Eq(Var("H2"), Remainder(Var("b"),IntNum(2))),
            Eq(Var("H3"), Remainder(Var("b"),Var("c"))),
//            Eq(Var("H4"), IntNum(0)),
            Eq(Var("H5"), Remainder(Var("c"),IntNum(2))),
//            Eq(Var("H6"), Remainder(Var("c"),Var("c"))),
//            Eq(Var("H7"), Remainder(Var("a"),Var("b"))),
            Eq(Var("result"),IntNum(0)),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Min und Max") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var("H1"), Min(Min(Var("a"),Var("b")),Var("c"))),
            Eq(Var("H2"), Min(Min(Var("b"),Var("c")),Var("a"))),
            Eq(Var("H3"), Min(Min(Var("c"),Var("a")),Var("b"))),
            Eq(Var("H4"), Min(Var("a"),Var("a"))),
            Eq(Var("c"),IntNum(2)),
            Eq(Var("H5"), Min(Var("c"),IntNum(2))),
            Eq(Var("H6"), Max(IntNum(16), Var("b"))),
            Eq(Var("H7"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("b")), Mul(IntNum(-1), IntNum(16))))),
            Eq(Var("H8"), Max(Var("b"),IntNum(16))),
            Eq(Var("H9"), Min(Var("a"),Add(Var("b"),Var("c")))),
            Eq(Var("H10"), Min(Add(Var("c"),Var("b")),Var("a"))),
            Eq(Var("result"), Add(Sub(Var("H10"),Var("H9")), Sub(Var("H9"), Var("H10"))))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(IntNum(2)))),
            Eq(Var("H1"), Min(IntNum(2),Min(Var("a"),Var("b")))),
//            Eq(Var("H2"), Min(Min(Var("b"),Var("c")),Var("a"))),
//            Eq(Var("H3"), Min(Min(Var("c"),Var("a")),Var("b"))),
//            Eq(Var("H4"), Var("a")),
//            Eq(IntNum(2), IntNum(2)),
//            Eq(Var("H5"), IntNum(2)),
            Eq(Var("H6"), Mul(IntNum(-1), Min(IntNum(-16), Mul(IntNum(-1), Var("b"))))),
//            Eq(Var("H7"), Mul(IntNum(-1), Min(Mul(IntNum(-1), Var("b")), Mul(IntNum(-1), IntNum(16))))),
            Eq(Var("H9"), Min(Var("a"),Add(IntNum(2),Var("b")))),
//            Eq(Var("H10"), Min(Add(Var("c"),Var("b")),Var("a"))),
            Eq(Var("result"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Neg") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var("H1"), Neg(Var("b"))),
            Eq(Var("H2"), Sub(IntNum(0), Var("b"))),
            Eq(Var("H3"), Neg(IntNum(-2))),
//            Eq(Var("a"), IntNum(2)),
            GT(Var("c"), Add(Add(Neg(IntNum(4)), Add(Var("a"),Var("b"))), Add(Neg(Var("a")), Neg(Var("b"))))),
            Eq(Var("H4"), Add(Add(Neg(IntNum(4)), Add(Var("a"), Var("b"))), Add(Var("a"), Neg(Var("c"))))),
            Eq(Var("H5"), Add(Add(Neg(IntNum(4)), Add(Var("a"), Var("b"))), Add(Var("a"), Mul(IntNum(-1), Var("c"))))),
            Eq(Var("H6"), Add(Var("a"), Neg(Var("c")))),
            Eq(Add(Neg(IntNum(0)), Sub(Var("H1"), Var("H2"))), Var(Name("result"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Call(Name("S3"), Seq(TermArg(Var("c")))),
            Eq(Var("H1"), Mul(IntNum(-1), Var("b"))),
//            Eq(Var(Name("H2")), Mul(IntNum(-1), Var("b"))),
//            Eq(Var("H3"), IntNum(2)),
//            Eq(Var("a"), IntNum(2)),
            LT(IntNum(-4), Var("c")),
            Eq(Var("H4"), Add(IntNum(-4), Add(Var("b"), Add(Mul(IntNum(-1), Var("c")), Mul(IntNum(2), Var("a")))))),
//            Eq(Var("H5"), Add(Add(Neg(IntNum(4)), Add(Var("a"), Var("b"))), Add(Var("a"), Mul(IntNum(-1), Var("c"))))),
            Eq(Var("H6"), Add(Var("a"), Mul(IntNum(-1), Var("c")))),
            Eq(Var("result"), IntNum(0)),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
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
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))), // now value of X also not known
            Eq(IntNum(12), Var("Z")),
            Eq(IntNum(12), Var("X")), // <- thus this var important for constraining value of Y
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("param$1")))),
            Eq(IntNum(12), Add(IntNum(2),Var("param$1"))),
            //            Eq(Var(Name("Z")), IntNum(12)),
//            Eq(IntNum(12), IntNum(12)),
            Eq(Var("param$0"), IntNum(12)),
//            Eq(Var("param$1"), Var("param$1"))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality with unknown val of var learned later") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(Var(Name("X")), Add(Var("Y"), IntNum(2))), // 2nd: dont replace rhs because Y still there, replace X with 12
            Eq(Var("W"), Add(Var("X"), IntNum(3))), // 2nd: x + 3 -> 12 + 3 -> V
            Eq(IntNum(12), Var(Name("X"))), // 1st: now value of X is known
            Eq(Var("V"), Add(Var("X"), IntNum(3))),
            Eq(IntNum(12), Var(Name("Z"))), // 1st: -> Z == X
            //            Eq(Var("V"), Var("Z")),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("param$1")))),
            Eq(IntNum(12), Add(IntNum(2),Var("param$1"))),
//            Eq(IntNum(15), IntNum(15)),
            //            Eq(Var(Name("Z")), IntNum(12))
//            Eq(IntNum(12), IntNum(12)),
            Eq(Var(Name("param$0")), IntNum(12)),
//            Eq(Var(Name("param$1")), Var(Name("param$1")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality with unknown val of var learned later 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(IntNum(12), Var(Name("X"))),
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("W"), Add(Var("X"), IntNum(3))),
            Eq(Var("V"), Add(Var("X"), IntNum(3))),
            Eq(IntNum(12), Var("Z")),
            //            Eq(Var("V"), Var("Z")),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Add(Var("Y"), IntNum(2)))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("Y")))),
            Eq(IntNum(12), Add(IntNum(2),Var("Y"))),
//            Eq(IntNum(15), IntNum(15)),
            //            Eq(Var(Name("Z")), IntNum(12))
            //            Eq(Var(Name("X")), IntNum(12)),
            Eq(Var("param$0"), IntNum(12)),
            Eq(Var("param$1"), Add(IntNum(2),Var("Y")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("m")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("m")), IntNum(11))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality with var bound in call used in term (with global propagation of leader)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(IntNum(12), Var("Z")),
            Eq(IntNum(12), Var("X")),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("param$1")))),
            //            Eq(IntNum(12), Add(IntNum(2),Var("param$1"))),
            //            Eq(Var(Name("Z")), IntNum(12)),
            //            Eq(IntNum(12), IntNum(12)),
            Eq(Var("param$0"), IntNum(12)),
            Eq(Var("param$1"), IntNum(10))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10))
          ))
        ))
      ))
    performTest(expected, input)
  }


  test("Call and 'assign' value to equal Var") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("A")),TermArg(Var("B")))),
            Eq(Var("param$0"), Var("A")),
            Eq(Var("param$0"), Add(IntNum(2), Var("B")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt),Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10)),
            Eq(Var("n"), IntNum(9))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(1)),
            Eq(Var("n"), IntNum(2))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call(Name("b"), Seq(TermArg(Var("param$0")),TermArg(Var("B")))),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$0"), Add(IntNum(2), Var("B")))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt),Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(10)),
            Eq(Var("n"), IntNum(9))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(1)),
            Eq(Var("n"), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Identity learned in 2nd pass") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), IntNum(2)),
            Eq(Var("C"), Sub(Var("A"),Var("B"))),
            Eq(Var("A"), IntNum(2)),
            Eq(Var("D"), IntNum(0)),
            Eq(Var("X"), Add(Var("C"), Var("D")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(IntNum(2)))),
            //            Eq(Var("B"), IntNum(2)),
//            Eq(IntNum(0), IntNum(0)),
//            Eq(IntNum(2), IntNum(2)),
            //            Eq(Var("D"), IntNum(0)),
            Eq(Var("X"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Calls: Equalities S(a),S(b),a+2,a==b,b+2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("H1"), Add(Var("a"), IntNum(2))), // finds equality -> H2 == b+2 but not removed (not removed by fix-point iteration because second Eq that is now binding H2 is comparison with unknown Var on rhs)
            Eq(Var("a"), Var("b")),
            Eq(Var("H2"), Add(Var("b"), IntNum(2))),
            Eq(Sub(Var("H2"), Var("H1")), Var("result")),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("H2"), Add(IntNum(2),Var("b"))),
//            Eq(Var("b"), Var("b")),
//            Eq(Var("H2"), Add(IntNum(2),Var("b"))), // removed by VN for Atoms
            Eq(Var("result"),IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("fix-point iteration: parameter constant in 2nd pass") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), IntNum(2)),
            Call(Name("S"), Seq(TermArg(Var("C")))), // not replaced with 0 in fix-point iteration since in first iteration replaced with parameter X which is then being bound here
            Eq(Var("X"), Var("C")),
            Eq(Var("X"), Sub(Var("A"), Var("B"))),
            Eq(Var("A"), IntNum(2)),
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(IntNum(2)))),
            Call(Name("S"), Seq(TermArg(Var("X")))),
            Eq(Var("X"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("fix-point iteration: variable constant in 2nd pass") { // result unsound without fix-point iteration since knowledge that C and X should equal 0 discarded
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("Result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), IntNum(2)),
            Call(Name("S"), Seq(TermArg(Var("C")))),
            Eq(Var("X"), Var("C")),
            Eq(Var("X"), Sub(Var("A"), Var("B"))),
            Eq(Var("A"), IntNum(2)),
            Eq(Var("Result"), IntNum(3)),
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("Result", TInt)), Seq(
          Body(Seq(
            Call(Name("S"), Seq(TermArg(IntNum(2)))),
            Call(Name("S"), Seq(TermArg(IntNum(0)))),
            Eq(Var("Result"), IntNum(3)),
          ))
        )),
        Relation(Name("S"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("defining term: A + 2 == A + 1 + 1") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), Add(Var("A"), IntNum(1))),
            Eq(Var("C"), Add(Var("B"), IntNum(1))),
            Eq(Var("D"), Add(Var("A"), IntNum(2))),
            Eq(Var("X"), Sub(Var("C"), Var("D")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), Add(IntNum(1),Var("A"))),
            Eq(Var("C"), Add(IntNum(2),Var("A"))),
//            Eq(Var("D"), Add(IntNum(2),Var("A"))),
            Eq(Var("X"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input, config=ConfigVN(useDefiningTerm=true))
  }

  test("defining term: (A * B) * C == A * (B * C)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Call(Name("S2"), Seq(TermArg(Var("B")))),
            Call(Name("S2"), Seq(TermArg(Var("C")))),
            Eq(Var("D"), Mul(Var("A"), Var("B"))),
            Eq(Var("E"), Mul(Var("B"), Var("C"))),
            Eq(Var("F"), Mul(Var("D"), Var("C"))), // Mul(Var("A"), Mul(Var("B"), Var("C")))
            Eq(Var("G"), Mul(Var("A"), Var("E"))), // Mul(Var("A"), Mul(Var("B"), Var("C")))
            Eq(Var("X"), Sub(Var("F"), Var("G")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Call(Name("S2"), Seq(TermArg(Var("B")))),
            Call(Name("S2"), Seq(TermArg(Var("C")))),
            Eq(Var("D"), Mul(Var("A"), Var("B"))),
            Eq(Var("E"), Mul(Var("B"), Var("C"))),
            Eq(Var("F"), Mul(Var("A"), Mul(Var("B"), Var("C")))), // TODO introduced new redundancy
//            Eq(Var("G"), Mul(Var("A"), Var("E"))),
            Eq(Var("X"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        )),
        Relation(Name("S3"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(2))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(4))
          ))
        ))
      ))
    performTest(expected, input, config=ConfigVN(useDefiningTerm=true))
  }

  test("defining term: large term through forward propagation") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), Add(Var("A"), IntNum(1))),
//            Eq(Var("C"), Add(Var("B"),Var("B"))),
            Eq(Var("X"), Mul(Var("B"), Add(IntNum(3),Var("A"))))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), Add(IntNum(1), Var("A"))),
//            Eq(Var("C"), Add(IntNum(2), Mul(IntNum(2), Var("A")))),
            Eq(Var("X"), Add(IntNum(3), Add(Var("A"), Add(Mul(IntNum(3), Var("A")), Mul(Var("A"),Var("A")))))), // TODO new large term and B not needed anymore...
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input, config = ConfigVN(useDefiningTerm = true)) 
  }

  test("defining term: problem with unbound var -> visit def term again") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Eq(Var("B"), Add(Var("A"), IntNum(1))),
            Eq(Var("C"), Add(Var("B"), IntNum(1))),
            Eq(Var("A"), Sub(Var("B"),Var("C"))),
            Eq(Var("D"), Add(Var("A"), IntNum(2))),
            Eq(Var("E"), Add(IntNum(-1), Add(Var("A"), Mul(IntNum(-1), Var("A"))))),  // (-1 + (A: <TInt> + (-1 * A: <TInt>)))
            Eq(Var("F"), Add(Add(IntNum(1), Var("A")), Mul(IntNum(-1), Add(IntNum(2), Var("A"))))), // ((1 + A: >TInt<) + (-1 * (2 + A: >TInt<)))
            Eq(Var("X"), Sub(Var("C"), Var("D")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(IntNum(-1)))),
            Eq(Var("X"), IntNum(0)),
//            Eq(IntNum(1), IntNum(1)),
//            Eq(IntNum(-1), IntNum(-1)),
//            Eq(IntNum(-1), IntNum(-1)),
            //            Eq(Var("D"), Add(IntNum(2),Var("A"))),
//            Eq(Var("E"), Add(IntNum(-1), Add(Var("A"), Mul(IntNum(-1), Var("A"))))),
//            Eq(Var("X"), Var("X"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input, config = ConfigVN(useDefiningTerm = true))
  }

  test("invalid constraint (-> should not be learned from)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
            Eq(Var("Y"), Var("X")),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
//          Body(Seq(
//            //            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
//            //            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
//            Eq(IntNum(1),IntNum(3)),                              // makes relation empty
//            Eq(Var(Name("param$0")), IntNum(3)),
//            Eq(Var(Name("param$1")), IntNum(1)),
//          ))
        ))
      ))
    performTest(expected, input)
  }

  test("wrong mode of type -> fixed") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("param$0")))),
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(0)),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("param$0")))),
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("wrong mode of type 2 -> fixed by typechecking again") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("A")))),
            Call(Name("S1"), Seq(TermArg(Var("B")))),
            Eq(Var("B"), Var("A")),
            Eq(Var("param$0"), Var("B")),
            Eq(Var("param$1"), IntNum(0)),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("param$0")))),
//            Call(Name("S1"), Seq(TermArg(Var("param$0")))), // removed by VN for Atoms
            Eq(Var("param$1"), IntNum(0)),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("wrong mode of type 3 -> fixed by typechecking again") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("H1"), Add(Var("a"), IntNum(2))),
            Eq(Var("a"), Var("b")),
            Eq(Var("H2"), Add(Var("b"), IntNum(2))), // NOT removed by using fix-point iteration because it becomes non binding comparison (containing an unknown Var)
            Call(Name("S2"), Seq(TermArg(Var("c")))),
            Eq(Var("H3"), Add(Var("b"), IntNum(2))),
            Eq(Var("a"), Var("c")),
            Eq(Var("c"), Var("b")),
            Eq(Sub(Var("H2"), Var("H1")), Var("result")),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("c")))),
            Call(Name("S2"), Seq(TermArg(Var("c")))),
            Eq(Var("H1"), Add(IntNum(2), Var("c"))),
//            Eq(Var("H1"), Add(IntNum(2), Var("c"))),  // removed by VN for Atoms
//            Call(Name("S2"), Seq(TermArg(Var("c")))), // removed by VN for Atoms
            //            Eq(Var("c"), Var("c")),
//            Eq(Var("H2"), Add(IntNum(2), Var("b"))),  // this gets removed in 1st phase since a == b already known
            Eq(Var("result"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("wrong mode of type? Learned that const in 2nd phase") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("H0"), Sub(Var("a"), Var("b"))),
            Eq(Var("H1"), Add(Var("a"), IntNum(2))),
            Eq(Var("a"), Var("b")),
            Eq(Var("H2"), Add(Var("b"), IntNum(2))), // NOT removed by using fix-point iteration because it becomes non binding comparison (containing an unknown Var)
            Eq(Add(Var("H2"), Var("H1")), Var("result")),
            Eq(Var("result"), Var("H0"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
//            Eq(Var("H0"), Sub(Var("a"), Var("b"))),
            Eq(Var("H2"), Add(IntNum(2), Var("b"))),
//            Eq(Var("a"), Var("b")),
//            Eq(Var("H2"), Add(IntNum(2), Var("b"))),  // removed by VN for Atoms
            Eq(Var("result"), Mul(IntNum(2), Var("H2"))),
            Eq(Var("result"), IntNum(0))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("test") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("c"), Add(Var("a"), Var("a"))),
            Eq(Var("a"),Var("c")),
            Eq(Var("result"), Add(Var("a"),Var("b")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("c")))),
            Call(Name("S2"), Seq(TermArg(Var("b")))),
            Eq(Var("c"), Mul(IntNum(2), Var("c"))),
//            Eq(Var("a"),Var("c")),
            Eq(Var("result"), Add(Var("b"),Var("c")))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        )),
        Relation(Name("S2"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(10))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(100))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("param bound in call: equal to other param") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("a"), Var("b"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Var("a"))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("param bound in call: equal to constant") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("a"), Var("b")),
            Eq(Var("a"), IntNum(123))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Var("a")),
            Eq(Var("a"), IntNum(123))
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("param bound in call: compare params") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Eq(Var("b"), IntNum(123)),
            Eq(Var("a"), IntNum(123)),
            Eq(Var("a"), Var("b")),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Eq(Var("b"), IntNum(123)),
            Eq(Var("a"), IntNum(123)),
            Eq(Var("a"), Var("b")),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("constraining variable bound in call") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("c"), IntNum(2)),
            Eq(Var("b"), IntNum(2)),
            Eq(Var("b"), Add(IntNum(3),Var("a"))),
            Eq(Var("X"), Add(Var("b"),Var("a"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
//            Eq(Var("c"), IntNum(2)),
//            Eq(Var("b"), IntNum(2)),
            Eq(IntNum(2), Add(IntNum(3),Var("a"))),
            Eq(Var("X"), Add(IntNum(2),Var("a"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Negated Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Eq(Var("b"), IntNum(1)),
            Call("S", Seq(TermArg(Var("a")),TermArg(Var("b"))), true),
            Call("S", Seq(TermArg(Var("a")),TermArg(Var("b")))),
            Call("S", Seq(TermArg(Var("c")),TermArg(Var("c"))), true),
            Eq(Var("c"), IntNum(2)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt),Param("b", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(1)),
            Eq(Var("a"), IntNum(0)),
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(1)),
            Eq(Var("b"), IntNum(0)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
//            Eq(Var("b"), IntNum(1)),
            Call("S", Seq(TermArg(Var("a")),TermArg(IntNum(1))), true),
            Call("S", Seq(TermArg(Var("a")),TermArg(IntNum(1)))),
            Call("S", Seq(TermArg(IntNum(2)),TermArg(IntNum(2))), true),
//            Eq(Var("c"), IntNum(2)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt),Param("b", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(1)),
            Eq(Var("a"), IntNum(0)),
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(1)),
            Eq(Var("b"), IntNum(0)),
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("precision double") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TDouble)), Seq(
          Body(Seq(
            Eq(Var("a"), DoubleNum(Double.MaxValue)),
            Eq(Var("b"), DoubleNum(Double.MinValue)),
            Eq(Var("c"), DoubleNum(1)),
            Eq(Var("d"), Add(Var("b"), Add(Var("a"), Var("c")))),
            Eq(Var("e"), Add(Add(Var("a"), Var("b")), Var("c"))),
            Eq(Var("X"), Add(Var("d"), Var("e"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TDouble)), Seq(
          Body(Seq(
//            Eq(Var("a"), DoubleNum(Double.MaxValue)),
//            Eq(Var("b"), DoubleNum(Double.MinValue)),
//            Eq(Var("c"), DoubleNum(1)),
            Eq(Var("d"), Add(DoubleNum(Double.MinValue), Add(DoubleNum(Double.MaxValue), DoubleNum(1)))),
            Eq(Var("e"), Add(Add(DoubleNum(Double.MaxValue), DoubleNum(Double.MinValue)), DoubleNum(1))),
            Eq(Var("X"), Add(Var("d"), Var("e"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("double") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TDouble)), Seq(
          Body(Seq(
            Call("S1", Seq(Var("a").arg)),
            Call("S1", Seq(Var("b").arg)),
            Call("S1", Seq(Var("c").arg)),
            Eq(Var("d"), Add(Var("b"), Add(Var("a"), Var("c")))),
            Eq(Var("e"), Add(Add(Var("a"), Var("b")), Var("c"))),
            Eq(Var("X"), Add(Var("d"), Var("e"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), DoubleNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), DoubleNum(5))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("X", TDouble)), Seq(
          Body(Seq(
            Call("S1", Seq(Var("a").arg)),
            Call("S1", Seq(Var("b").arg)),
            Call("S1", Seq(Var("c").arg)),
            Eq(Var("d"), Add(Var("b"), Add(Var("a"), Var("c")))),
            Eq(Var("e"), Add(Add(Var("a"), Var("b")), Var("c"))),
            Eq(Var("X"), Add(Var("d"), Var("e"))),
          ))
        )),
        Relation(Name("S1"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("param$0")), DoubleNum(1))
          )),
          Body(Seq(
            Eq(Var(Name("param$0")), DoubleNum(5))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Add DoubleNum") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble), Param("param$2", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), DoubleNum(1.2)),
            Eq(Var(Name("Y")), DoubleNum(3.4)),
            Eq(Var(Name("H1")), Add(Var("X"), DoubleNum(2.4))),
            Eq(Var(Name("H2")), Add(DoubleNum(2.4), Var("X"))),
            Eq(Var(Name("H3")), Add(DoubleNum(2.4), DoubleNum(0))),
            Eq(Var(Name("H4")), Add(Var(Name("H3")), DoubleNum(0.0))),
            Eq(Var(Name("Z")), Add(Var("H2"), Var("H4"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$2")), Var(Name("Z")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble), Param("param$2", TDouble)), Seq(
          Body(Seq(
            //            Eq(Var(Name("X")), DoubleNum(1.2)),
            //            Eq(Var(Name("Y")), DoubleNum(3.4)),
            //            Eq(Var(Name("H1")), Add(DoubleNum(2.4),Var("X"))),
            //            Eq(Var(Name("H2")), Add(DoubleNum(2.4), Var("X"))),
            //            Eq(Var(Name("H3")), DoubleNum(2.4)),
            //            Eq(Var(Name("H4")), Add(Var(Name("H2")), DoubleNum(0.0))),
            //            Eq(Var(Name("Z")), Add(Var("H1"), Var("H3"))),
            Eq(Var(Name("param$0")), DoubleNum(1.2)),
            Eq(Var(Name("param$1")), DoubleNum(3.4)),
            Eq(Var(Name("param$2")), DoubleNum(6))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(normalizeDoubles=true))
  }

  test("sub DoubleNum") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), DoubleNum(2)),
            Eq(Var(Name("H1")), Sub(Var(Name("X")), Add(Var("X"), DoubleNum(4)))),
            Eq(Var(Name("H2")), DoubleNum(-4)),
            Eq(Var(Name("Y")), Add(Var("H1"), Var("H2"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble)), Seq(
          Body(Seq(
            //            Eq(Var(Name("X")), DoubleNum(2)),
            //            Eq(Var(Name("H1")), DoubleNum(-4)),
            //            Eq(Var(Name("H2")), IntNum(-4)),
            //            Eq(Var(Name("Y")), Mul(DoubleNum(2), Var("H1"))),
            Eq(Var(Name("param$0")), DoubleNum(2)),
            Eq(Var(Name("param$1")), DoubleNum(-8))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(normalizeDoubles=true))
  }

  test("mul DoubleNum") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), DoubleNum(3)),
            Eq(Var(Name("H1")), Mul(DoubleNum(2), Mul(Var(Name("X")), DoubleNum(3)))),
            Eq(Var(Name("H2")), Mul(Mul(DoubleNum(2), Var(Name("X"))), DoubleNum(3))),
            Eq(Var(Name("Y")), Mul(Var("H1"), Mul(Var("H2"), DoubleNum(1)))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble)), Seq(
          Body(Seq(
            //            Eq(Var(Name("X")), DoubleNum(3)),
            //            Eq(Var(Name("H1")), Mul(DoubleNum(6), Var(Name("X")))),
            //Eq(Var(Name("H2")), Mul(Mul(IntNum(2), Var(Name("X"))), IntNum(3))),
            //            Eq(Var(Name("Y")), Mul(Var("H1"), Var("H1"))),
            Eq(Var(Name("param$0")), DoubleNum(3)),
            Eq(Var(Name("param$1")),  DoubleNum(18*18))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(normalizeDoubles=true))
  }

  test("div DoubleNum") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble), Param("param$2", TDouble)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Div(DoubleNum(2), DoubleNum(1))),
            Eq(Var(Name("H1")), DoubleNum(2)),
            Eq(Var(Name("H2")), Div(DoubleNum(4), DoubleNum(2))),
            Eq(Var(Name("H3")), Div(DoubleNum(1), DoubleNum(2))),
            Eq(Var(Name("H4")), Div(Var("H1"), Var("H3"))),
            Eq(Var(Name("H5")), Mul(Var("X"), Var("X"))),
            Eq(Var(Name("H6")), Div(Var("X"), DoubleNum(1))),
            Eq(Var(Name("H7")), Div(Add(Var("X"), DoubleNum(0)), DoubleNum(2))),
            Eq(Var(Name("H8")), Add(Div(Var("X"), DoubleNum(2)), Div(DoubleNum(0), DoubleNum(2)))),
            Eq(Div(Var("X"), DoubleNum(2)), Var("H8")),
            Eq(Var("Y"), DoubleNum(1)),
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
        Relation(Name("a"), Seq(Param("param$0", TDouble), Param("param$1", TDouble), Param("param$2", TDouble)), Seq(
          Body(Seq(
            //            Eq(Var(Name("X")), DoubleNum(2)),
            //            Eq(Var(Name("H1")), IntNum(2)),
            //            Eq(Var(Name("H2")), Div(IntNum(4), IntNum(2))),
            //            Eq(Var(Name("H3")), DoubleNum(0.5)),
            //            Eq(Var(Name("H4")), Div(Var("X"), Var("H3"))),
            //            Eq(Var(Name("H5")), Mul(Var("X"), Var("X"))),
            //            Eq(Var(Name("H6")), Div(Var("X"), IntNum(1))),
            //            Eq(Var(Name("H7")), DoubleNum(1)),
            //            Eq(Var(Name("H8")), Add(Div(Var("X"), IntNum(2)), Div(IntNum(0), IntNum(2)))),
            //            Eq(DoubleNum(1), DoubleNum(1)),
            //            Eq(Var("Y"), DoubleNum(1)),
            //            Eq(Var("Z"), Div(Var("H2"), Var("H1"))),
            Eq(Var(Name("param$0")), DoubleNum(2)),
            Eq(Var(Name("param$1")), DoubleNum(1)),
            Eq(Var(Name("param$2")), DoubleNum(1))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(normalizeDoubles=true))
  }

  test("Global propagation of leader") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(),TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt),Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123)),
            Eq(Var("a"), Var("n")),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), Add(Var("a"), IntNum(23)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(IntNum(123)))),
//            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), IntNum(125)),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt),Param("n", TInt)), Seq(
          Body(Seq(
//            Eq(Var("a"), IntNum(123)),
            Eq(Var("n"), IntNum(123)),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
//            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Global propagation of leader (parameter not replaced)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(Var("param$1")))),
            Eq(Var("param$1"), Var("param$0"))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123)),
            Eq(Var("a"), Var("n")),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), Add(Var("a"), IntNum(23)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(Var("param$1")))),
            Eq(Var("param$0"), Var("param$1"))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            //            Eq(Var("a"), IntNum(123)),
            Eq(Var("n"), IntNum(123)),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            //            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Global propagation of leader (invalid body)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), IntNum(0)),
            Eq(Var("param$1"), Var("Y"))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123)),
            Eq(Var("a"), Var("n")),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), Add(Var("a"), IntNum(23)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            //            Eq(Var("a"), IntNum(123)),
            Eq(Var("n"), IntNum(123)),
            Eq(Var("m"), IntNum(10))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            //            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Global propagation of leader (invalid body with constant argument)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(IntNum(1)))), // 1 not in Relation S
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Global propagation of leader (invalid body 3)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(Var("b")))),
            Call("S", Seq(TermArg(Add(Var("b"), IntNum(2))))), // not in Relation S
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Global propagation of leader (circular dependence)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("m")), TermArg(Var("a")))),
            Eq(Var("a"), Var("n")),
            Eq(Var("m"), IntNum(2))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), Sub(Var("a"), IntNum(100)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(IntNum(0)))),
//            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), IntNum(2)),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("m")), TermArg(IntNum(0)))),
            Eq(Var("n"), IntNum(0)),
            Eq(Var("m"), IntNum(2))
          )),
          Body(Seq(
            Eq(Var("m"), IntNum(0)),
//            Eq(Var("a"), IntNum(100)),
            Eq(Var("n"), IntNum(0))
          ))
        ))
      ))
    performTest(expected, input)
  }


  test("Global propagation of leader (recursion)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), IntNum(0))
          )),
          Body(Seq(
            Call("R", Seq(WildcardArg(), TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(1)),
            Eq(Var("n"), IntNum(100))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(WildcardArg(), TermArg(IntNum(100)))),
//            Eq(Var("X"), IntNum(102)),
            Eq(Var("param$0"), IntNum(102)),
            Eq(Var("param$1"), IntNum(0))
          )),
          Body(Seq(
            Call("R", Seq(WildcardArg(), TermArg(IntNum(0)))),
//            Eq(Var("X"), Add(Var("Y"), IntNum(2))),
            Eq(Var("param$0"), IntNum(2)),
            Eq(Var("param$1"), IntNum(0))
          ))
        )),
        Relation(Name("S"), Seq(Param("m", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var("m"), IntNum(1)),
            Eq(Var("n"), IntNum(100))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Path Example") {
    val input = IRModule("Test3", Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})), Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("path", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("path", Seq(IntNum(1), Var("y")))
        )),
      )).addHint(MainHint),
    ))
    val expected = IRModule("Test3", Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})), Seq(
      Relation("edge", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Eq(Var("x"), IntNum(1)),
          Eq(Var("y"), IntNum(2))
        )),
        Body(Seq(
          Eq(Var("x"), IntNum(2)),
          Eq(Var("y"), IntNum(3))
        ))
      )),
      Relation("path", Seq(
        Param("x", TInt),
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("edge", Seq(Var("x"), Var("y")))
        )),
        Body(Seq(
          Call("path", Seq(Var("x"), Var("z"))),
          Call("path", Seq(Var("z"), Var("y"))),
        ))
      )),
      Relation("main", Seq(
        Param("y", TInt)
      ), Seq(
        Body(Seq(
          Call("path", Seq(IntNum(1), Var("y")))
        )),
      )).addHint(MainHint),
    ))
    performTest(expected, input)
  }

  test("Cast: Double to Int -> no constant") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), Cast(DoubleNum(123), TInt)),
            Eq(Var("param$0"), Var("a"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), Cast(DoubleNum(123), TInt)),
//            Eq(Var("param$0"), Var("a"))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Cast: Int to Int -> normalize Cast away") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), Cast(IntNum(123), TInt)),
            Eq(Var("param$0"), Var("a"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            //            Eq(Var("a"), Cast(IntNum(123), TInt)),
            Eq(Var("param$0"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Cast: Double to Double -> normalize Cast away") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
            Eq(Var("a"), Cast(DoubleNum(123), TDouble)),
            Eq(Var("param$0"), Var("a"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
//            Eq(Var("a"), Cast(DoubleNum(123), TDouble)),
            Eq(Var("param$0"), DoubleNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Cast: No change") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("param$0"), Cast(DoubleNum(123), TInt)),
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("a"), Cast(IntNum(0), TDouble)),
            Eq(Var("param$0"), IntNum(1))
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("param$0"), Cast(Var("a"), TInt))
          )),
        )),
        Relation(Name("S"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
            Eq(Var("param$0"), DoubleNum(1))
          )),
          Body(Seq(
            Eq(Var("param$0"), DoubleNum(0))
          )),
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("param$0"), Cast(DoubleNum(123), TInt)),
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("a"), Cast(IntNum(0), TDouble)),
            Eq(Var("param$0"), IntNum(1))
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("param$0"), Cast(Var("a"), TInt))
          )),
        )),
        Relation(Name("S"), Seq(Param("param$0", TDouble)), Seq(
          Body(Seq(
            Eq(Var("param$0"), DoubleNum(1))
          )),
          Body(Seq(
            Eq(Var("param$0"), DoubleNum(0))
          )),
        ))
      ))
    performTest(expected, input)
  }

}
