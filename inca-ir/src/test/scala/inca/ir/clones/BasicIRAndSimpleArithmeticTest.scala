package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.valueNumbering.ValueNumbering


// tests with DoubleNum are in ArithmeticTest.scala

/** tests BasicValueNumbering without any additional functions for extensions */
class BasicIRAndSimpleArithmeticTest extends ValueNumberingTestAbstract {

  override def performTest(expected: IRModule, input: IRModule, config: ConfigVN): Unit = {
    super.performTestWithBaseVN(expected, input, config)
  }


  test("Simple Redundant term in Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), IntNum(1)),
            Eq(Var(Name("Y")), IntNum(1)),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y"))),
            Eq(Var(Name("param$1")), Var(Name("param$0")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("X")), IntNum(1)),
//            //Eq(Var(Name("Y")), Var(Name("X"))),
//            Eq(Var(Name("param$0")), Var(Name("X"))),
//            Eq(Var(Name("param$1")), Var(Name("X"))),
//            Eq(Var("param$1"),Var("param$0"))
//          ))
          Body(Seq(
            Eq(Var(Name("param$0")), IntNum(1)),
            //Eq(Var(Name("Y")), Var(Name("X"))),
//            Eq(Var(Name("param$0")), Var("param$0")),
            Eq(Var(Name("param$1")), Var("param$0")),
            Eq(Var("param$1"),Var("param$0"))
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
            Eq(Var(Name("H1")), Add(Var("X"), IntNum(2))), // DONT REMOVE this -> block removal in 2nd phase
            Eq(Var(Name("H2")), Add(Var("X"), IntNum(2))), // remove because value already available through H1 (1st phase)
            Eq(Var(Name("Z")), Add(Var("H1"), Var("H2"))), // -> H1 + H1
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
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(3)),
            Eq(Var("H1"), Add(Var("param$0"), IntNum(2))),
            //Eq(Var(Name("H2")), Var(Name("H1"))),
            Eq(Var("param$2"), Add(Var("H1"), Var("H1"))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$1"), Var("param$1")),
//            Eq(Var("param$2"), Var("param$2"))
          ))
        ))
      ))
    performTest(expected,input)
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
            Eq(Var("param$0"), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            //Eq(Var(Name("Y")), Mul(IntNum(1), Add(IntNum(2), IntNum(3)))),
            Eq(Var("H1"), Div(Mul(Var("param$0"), Var("param$0")), IntNum(2))),
            //Eq(Var(Name("H2")), Div(Mul(Var("X"), Var("Y")), IntNum(2))),
            Eq(Var("H3"), Sub(Var("H1"), Mul(Var("H1"), IntNum(3)))),
            Eq(Var("param$2"), Sub(Var("H1"), Var("param$0"))),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0")),
//            Eq(Var("param$2"), Var("param$2"))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Redundant term in Eq: Two Bodies") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(2), IntNum(3))),
            Eq(Var(Name("H1")), Div(Var("X"), IntNum(2))),
            Eq(Var(Name("param$0")), Var(Name("H1")))
          )),
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(2), IntNum(3))),
            Eq(Var(Name("B")), Div(Var("X"), IntNum(2))),  // should not be replaced with H1 from other body
            Eq(Var(Name("param$0")), Add(Var("B"),IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("X"), Mul(IntNum(2), IntNum(3))),
            Eq(Var("param$0"), Div(Var("X"), IntNum(2))),
//            Eq(Var("param$0"), Var("param$0"))
          )),
          Body(Seq(
            Eq(Var(Name("X")), Mul(IntNum(2), IntNum(3))),
            Eq(Var(Name("B")), Div(Var("X"), IntNum(2))),  // should not be replaced with H1 from other body
            Eq(Var(Name("param$0")), Add(Var("B"),IntNum(1)))
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
            Eq(Var("param$0"), IntNum(1)),
//            Eq(Var(Name("B")), IntNum(1)),
//            Eq(Var(Name("C")), Var("B")),
//            Eq(Var(Name("D")), Var("A")),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var(Name("E")), IntNum(1)),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var(Name("param$0")), Var("param$0")),
            Eq(Var(Name("param$1")), Var("param$0"))
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
            Eq(Var(Name("E")), Mul(IntNum(1), IntNum(1))),
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
            Eq(Var("param$0"), IntNum(1)),
//            Eq(Var(Name("B")), IntNum(1)),
//            Eq(Var(Name("C")), Var("B")),
//            Eq(Var(Name("D")), Var("A")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$0"), Mul(Var("param$0"), Var("param$0"))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
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
            Eq(Var("X1"), IntNum(1)),
            Eq(IntNum(1), Var("X2")),
            Eq(Var("X2"), Var("X3")),
            Eq(Var("param$0"), Var("X1")),
            Eq(Var("param$1"), Var("X3"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1)),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$1")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
          ))
        ))
      ))
    performTest(expected, input)
  }

  // TODO concludes wrong equality from comparison eq... -> wrong replacement in 2nd phase
  //    (but assumption that no unsatisfiable atoms are included)
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
////            Eq(Var(Name("X")), Add(IntNum(2), IntNum(1))),
////            Eq(Var(Name("Y")), Sub(IntNum(2), IntNum(1))),
////            Eq(Var(Name("Z")), Var(Name("X"))),
//            Eq(Sub(IntNum(2), IntNum(1)), Add(IntNum(2), IntNum(1))),     // should stay since it makes relation empty
//            Eq(Var(Name("param$0")), Add(IntNum(2), IntNum(1))),
//            Eq(Var(Name("param$1")), Sub(IntNum(2), IntNum(1))),
//            Eq(Var(Name("param$2")), Add(IntNum(2), IntNum(1)))
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
            Eq(Var(Name("main_result$0")), IntNum(7)),
//            Eq(Var(Name("if_result$0")), Var(Name("x"))),
//            Eq(Var(Name("main_result$0")), Var(Name("main_result$0"))),
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
            Eq(Var("A"), Add(IntNum(2), IntNum(3))),
            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
            Eq(Var("C"), Var("B")),
            Call("b", Seq(TermArg(Var("A")), TermArg(Var("B")), TermArg(Var("C"))), false),
            Call("b", Seq(TermArg(IntNum(1)), TermArg(Var("B")), TermArg(Var("B"))), false),
            Eq(Var("param$0"), Var("A")),
            Eq(Var("param$1"), Var("C"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(5)),
            Eq(Var("param$2"), IntNum(5)),
          )),
          Body(Seq(
            Eq(Var("param$0"), IntNum(5)),
            Eq(Var("param$1"), IntNum(5)),
            Eq(Var("param$2"), IntNum(5)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), Add(IntNum(2), IntNum(3))),
//            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
//            Eq(Var("C"), Var("B")),
            Call("b", Seq(TermArg(Var("param$0")), TermArg(Var("param$0")), TermArg(Var("param$0"))), false),
            Call("b", Seq(TermArg(IntNum(1)), TermArg(Var("param$0")), TermArg(Var("param$0"))), false),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(5)),
            Eq(Var("param$2"), Var("param$1")),
          )),
          Body(Seq(
            Eq(Var("param$0"), IntNum(5)),
            Eq(Var("param$1"), Var("param$0")),
            Eq(Var("param$2"), Var("param$0")),
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
            Call("b", Seq(TermArg(Var("X1")))),
            Eq(Var("X2"), Var("X1")),
            Eq(Var("param$0"), Var("X1")),
            Eq(Var("param$1"), Var("X2"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b", Seq(TermArg(Var("param$0")))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
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
            Call("b",Seq(TermArg(Var("X")))),
            Call("b",Seq(TermArg(Var("Y")))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("param$0"), IntNum(2))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b",Seq(TermArg(Var("param$0")))),
            Call("b",Seq(TermArg(Var("param$1")))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$1"), Var("param$1"))
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
            Eq(Var("A"), Add(IntNum(2), IntNum(3))),
            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
            Call("b", Seq(TermArg(Var("X")),TermArg(Var("B")))),
            Call("b", Seq(TermArg(Var("Y")),TermArg(Var("B")))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(5))
          )),
          Body(Seq(
            Eq(Var("param$0"), IntNum(2)),
            Eq(Var("param$1"), IntNum(2))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("A"), Add(IntNum(2), IntNum(3))),
//            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
            Call("b", Seq(TermArg(Var("param$0")),TermArg(Var("A")))),
            Call("b", Seq(TermArg(Var("param$1")),TermArg(Var("A")))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$1"), Var("param$1"))
          ))
        )),
        Relation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(1)),
            Eq(Var("param$1"), IntNum(5))
          )),
          Body(Seq(
            Eq(Var("param$0"), IntNum(2)),
            Eq(Var("param$1"), Var("param$0"))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Ext Call replace Args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("A"), Add(IntNum(2), IntNum(3))),
            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
            Eq(Var("C"), Var("B")),
            ExtensionalCall("b", Seq(TermArg(Var("A")), TermArg(Var("B")), TermArg(Var("C")))),
            ExtensionalCall("b", Seq(TermArg(IntNum(1)), TermArg(Var("B")), TermArg(Var("B")))),
            Eq(Var("param$0"), Var("A")),
            Eq(Var("param$1"), Var("C"))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("param$2", TInt)))
      ))

    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), Add(IntNum(2), IntNum(3))),
//            Eq(Var("B"), Add(IntNum(2), IntNum(3))),
//            Eq(Var("C"), Var("B")),
            ExtensionalCall("b", Seq(TermArg(Var("param$0")), TermArg(Var("param$0")), TermArg(Var("param$0")))),
            ExtensionalCall("b", Seq(TermArg(IntNum(1)), TermArg(Var("param$0")), TermArg(Var("param$0")))),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
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
            ExtensionalCall(Name("b"), Seq(TermArg(Var("X1")))),
            Eq(Var("X2"), Var("X1")),
            Eq(Var(Name("param$0")), Var("X1")),
            Eq(Var(Name("param$1")), Var("X2"))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            ExtensionalCall(Name("b"), Seq(TermArg(Var("param$0")))),
            //            Eq(Var("X2"), Var("X1")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
          ))
        )),
        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
      ))
    performTest(expected, input)
  }

  test("Call and check for Equality") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(IntNum(8), IntNum(2))),
            Eq(Var("Y"), Add(IntNum(8), IntNum(2))),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
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
            Call("b", Seq(TermArg(Var("param$0")))),
            Eq(Var("param$0"), Add(IntNum(8), IntNum(2))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
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

  test("Call and check for Equality with switched args") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(IntNum(8), IntNum(2))),
            Eq(Add(IntNum(8), IntNum(2)), Var("Y")),
            Eq(Var("param$0"), Var("X")),
            Eq(Var("param$1"), Var("Y"))
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
            Call("b", Seq(TermArg(Var("param$0")))),
            Eq(Var("param$0"), Add(IntNum(8), IntNum(2))),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Var("param$0"))
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
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))), // now value of X also not known
            Eq(IntNum(12), Var("Z")),
            Eq(IntNum(12), Var("X")), // <- thus this var shouldn`t be replaced either
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
            Call("b", Seq(TermArg(Var("param$1")))),
            Eq(Var("param$0"), Add(Var("param$1"), IntNum(2))), // now value of X also not known
            Eq(Var("param$0"), IntNum(12)),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$1"), Var("param$1"))
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

  test("Call and check for Equality with unknown val of var learned later") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
          Body(Seq(
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("X"), Add(Var("Y"), IntNum(2))), // 2nd: dont replace lhs because Y still there, replace X with 12
            Eq(Var("W"), Add(Var("X"), IntNum(3))),       // 2nd: x + 3 -> 12 + 3 -> V
            Eq(IntNum(12), Var("X")),               // 1st: now value of X is known -> W also known
            Eq(Var("V"), Add(Var("X"), IntNum(3))), // whether redundancy recognized in 1st pass determined by which id used (of original term or newTerm with visited subterms)
            Eq(IntNum(12), Var("Z")),               // 1st: -> Z == X
            //            Eq(Var("V"), Var("Z")),
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
            Call("b", Seq(TermArg(Var("param$1")))),
            Eq(Var("param$0"), Add(Var("param$1"), IntNum(2))),
            Eq(Var("W"), Add(Var("param$0"), IntNum(3))),
            Eq(Var("param$0"),IntNum(12)),
//            Eq(Var("V"), Add(Var("X"), IntNum(3))),
//            Eq(IntNum(12), Var("Z")),
            //            Eq(Var("V"), Var("Z")),
//            Eq(Var("param$0"), Var("param$0")),
//            Eq(Var("param$1"), Var("param$1"))
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

  test("Call and check for Equality with unknown val of var learned later2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("W", TInt)), Seq(
          Body(Seq(
            Eq(IntNum(12), Var("X")),
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
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt), Param("W", TInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), IntNum(12)),
            Call("b", Seq(TermArg(Var("Y")))),
            Eq(Var("param$0"), Add(Var("Y"), IntNum(2))),
            Eq(Var("W"), Add(Var("param$0"), IntNum(3))),
//            Eq(Var("V"), Add(Var("X"), IntNum(3))),
//            Eq(Var("Z"), IntNum(12)),
            //            Eq(Var("V"), Var("Z")),
//            Eq(Var("param$0"), Var("param$0")),
            Eq(Var("param$1"), Add(Var("Y"), IntNum(2)))
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


//  test("Simple Redundant Call") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Eq(Var(Name("param$0")), Var(Name("X1")))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(1))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            //Call(Name("b"), Seq(TermArg(Var(Name("X1"))), false),
//            Eq(Var(Name("param$0")), Var(Name("X1")))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(1))
//          ))
//        ))
//      ))
//    performTest(expected, input)
//  }
//
//
//  test("Redundant Calls") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Eq(Var("X1"), Var("X2")),
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
//            Eq(Var(Name("param$0")), Var(Name("X2")))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(1))
//          )),
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(11))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            //            Eq(Var("X1"), Var("X2")),
//            //            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            //            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
//            Eq(Var(Name("param$0")), Var(Name("X1")))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(1))
//          )),
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(11))
//          ))
//        ))
//      ))
//    performTest(expected, input)
//  }
//
//  test("Redundant Ext Calls") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            Eq(Var("X1"), Var("X2")),
//            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
//            Eq(Var(Name("param$0")), Var(Name("X2")))
//          ))
//        )),
//        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            ExtensionalCall(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            //            Eq(Var("X1"), Var("X2")),
//            //            Call(Name("b"), Seq(TermArg(Var(Name("X1")))), false),
//            //            Call(Name("b"), Seq(TermArg(Var(Name("X2")))), false),
//            Eq(Var(Name("param$0")), Var(Name("X1")))
//          ))
//        )),
//        ExtensionalRelation(Name("b"), Seq(Param("param$0", TInt)))
//      ))
//    performTest(expected, input)
//  }
//
//  test("Redundant Atoms") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("X1"), IntNum(32)),
//            Eq(Var("X1"), Var("X2")),
//            Eq(Var("X2"), Var("X1")),
//            GE(Var("X1"),IntNum(2)),
//            GE(Var("X2"),IntNum(2)),
//            LT(Var("X1"), IntNum(64)),
//            LT(Var("X2"), IntNum(64)),
//            Eq(Var("X1"), IntNum(4), true),
//            Eq(Var("X2"), IntNum(4), true),
//            Eq(Var(Name("param$0")), Var(Name("X2")))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("X1"), IntNum(32)),
////            Eq(Var("X1"), Var("X2")),
////            Eq(Var("X1"), Var("X1")),
//            GE(Var("X1"), IntNum(2)),
////            GE(Var("X2"), IntNum(2)),
//            LT(Var("X1"), IntNum(64)),
////            LT(Var("X2"), IntNum(64)),
//            Eq(Var("X1"), IntNum(4), true),
////            Eq(Var("X2"), IntNum(4), true),
//            Eq(Var(Name("param$0")), Var(Name("X1")))
//          ))
//        ))
//      ))
//    performTest(expected, input)
//  }
//
//  test("Redundant bodies") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"),IntNum(1)))
//          )),
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
////          Body(Seq(
////            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
////            GT(Var(Name("n")), IntNum(0)),
////            Eq(Var(Name("result")), Add(Var("n", Int(1))))
////          )),
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  // This does not work, since currently not known whether the order of atoms can be switched without changing the meaning of the program
//  test("Redundant bodies 2") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
//            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
//          )),
//          Body(Seq(
//            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
////          Body(Seq(
////            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
////            Eq(Var(Name("m")), Mul(IntNum(2), IntNum(2))),
////            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
////          )),
//          Body(Seq(
//            Eq(Var(Name("m")), IntNum(4)),
//            Eq(Var(Name("n")), IntNum(10)),
//            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  test("Redundant Relation") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  test("Redundant Relation with renaming") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("m", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("m")), IntNum(10)),
//            GT(Var(Name("m")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("m"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("param$0", TInt), Param("param$1", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("param$0")), IntNum(10)),
//            GT(Var(Name("param$0")), IntNum(0)),
//            Eq(Var(Name("param$1")), Add(IntNum(1), Var("param$0")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(normalize = true, attemptAlphaEquivalence = true))
//  }
//
//  test("Redundant Relation with Call of removed Relation") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("result")),TermArg(Var("n"))))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("b"), Seq(TermArg(Var("result")),TermArg(Var("n"))))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10)),
//            GT(Var(Name("n")), IntNum(0)),
//            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  test("Repeated Atoms in different Relations") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("B1"), IntNum(2))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("A1")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(IntNum(2),Var("A1"))),
//            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  test("Repeated Atoms in different Relations 2") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("m", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(IntNum(2),Var("B1"))),
//            Eq(Var(Name("m")), Mul(Var("B1"),Var("n"))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("A1")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1"))),
//            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("m", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("A1")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(IntNum(2),Var("A1"))),
//            Eq(Var(Name("m")), Mul(Var("A1"),Var("n"))),
//            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//
//  test("Repeated Atoms in different Relations with Calls") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Eq(Var(Name("A2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("A1"),Var("A2"))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("B1")))),
//            Eq(Var(Name("B2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("B1"),Var("B2"))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10))
//          )),
//          Body(Seq(
//            Eq(Var("n"), IntNum(11))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Eq(Var(Name("A2")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(Var("A1"),Var("A2"))),
//            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
//          ))
//        )),
//        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10))
//          )),
//          Body(Seq(
//            Eq(Var("n"), IntNum(11))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }
//
//  test("Repeated Atoms in same Relation with Calls") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Eq(Var(Name("A2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          )),
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("B1")))),
//            Eq(Var(Name("B2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
//            Eq(Var(Name("n")), Mul(Var("B1"), Var("B2"))),
//            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
//          ))
//        )),
//        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10))
//          )),
//          Body(Seq(
//            Eq(Var("n"), IntNum(11))
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("c"), Seq(TermArg(Var("A1")))),
//            Eq(Var(Name("A2")), IntNum(10)),
//            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
//            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
//          ))//,
////          Body(Seq(
////            Call(Name("c"), Seq(TermArg(Var("A1")))),
////            Eq(Var(Name("A2")), IntNum(10)),
////            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
////            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
////          ))
//        )),
//        Relation(Name("c"), Seq(Param("n", TInt)), Seq(
//          Body(Seq(
//            Eq(Var(Name("n")), IntNum(10))
//          )),
//          Body(Seq(
//            Eq(Var("n"), IntNum(11))
//          ))
//        ))
//      ))
//    performTest(expected, input, ConfigVN(true))
//  }

}
