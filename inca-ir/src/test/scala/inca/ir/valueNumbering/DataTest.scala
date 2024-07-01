package inca.ir.valueNumbering

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, data, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}


class DataTest extends ValueNumberingTestAbstract {

  test("Construct") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List"))), Seq(
          Body(Seq(
            Eq(Var("tempRes"), Construct(Name("Nil"),Seq())),
            Eq(Var("res"), Var("tempRes"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List"))), Seq(
          Body(Seq(
//            Eq(Var("tempRes"), Construct(Name("Nil"),Seq())),
            Eq(Var("res"), Construct(Name("Nil"),Seq()))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Deconstruct 1") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")),Param("num", TInt)), Seq(
          Body(Seq(
            Eq(Var("tempRes"), Construct(Name("Cons"),Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Var("tempRes")),
            Deconstruct(Var("tempRes"), RefByName(Name("Cons")), Seq(TermArg(Var("head")), TermArg(Var("tail"))), false),
            Eq(Var("num"), Var("head"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")),Param("num", TInt)), Seq(
          Body(Seq(
//            Eq(Var("res"), Construct(Name("Cons"),Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Construct(Name("Cons"),Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Deconstruct(Construct(Name("Cons"),Seq(IntNum(123), Construct(Name("Nil"), Seq()))), RefByName(Name("Cons")), Seq(TermArg(Var("num")), TermArg(Var("tail"))), false),
//            Eq(Var("num"), Var("head"))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Deconstruct 2: param bound in deconstruct and param = int") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")), Param("num", TInt)), Seq(
          Body(Seq(
            Eq(Var("tempRes"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Var("tempRes")),
            Deconstruct(Var("tempRes"), RefByName(Name("Cons")), Seq(TermArg(Var("num")), TermArg(Var("tail"))), false),
            Eq(Var("num"), IntNum(123))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")), Param("num", TInt)), Seq(
          Body(Seq(
//            Eq(Var("tempRes"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Deconstruct(Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq()))), RefByName(Name("Cons")), Seq(TermArg(Var("num")), TermArg(Var("tail"))), false),
            Eq(Var("num"), IntNum(123))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Deconstruct 3: param bound in deconstruct and param = param") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")), Param("num1", TInt), Param("num2", TInt)), Seq(
          Body(Seq(
            Eq(Var("tempRes"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Var("tempRes")),
            Deconstruct(Var("tempRes"), RefByName(Name("Cons")), Seq(TermArg(Var("num1")), TermArg(Var("tail"))), false),
            Eq(Var("num1"), Var("num2"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {}, new data.IR {})),
      Seq(
        DataDefinition("List"),
        CaseDefinition("Nil", Seq(), TData("List")),
        CaseDefinition("Cons", Seq(TInt, TData("List")), TData("List")),
        Relation(Name("a"), Seq(Param("res", TData("List")), Param("num1", TInt), Param("num2", TInt)), Seq(
          Body(Seq(
//            Eq(Var("tempRes"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Eq(Var("res"), Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq())))),
            Deconstruct(Construct(Name("Cons"), Seq(IntNum(123), Construct(Name("Nil"), Seq()))), RefByName(Name("Cons")), Seq(TermArg(Var("num1")), TermArg(Var("tail"))), false),
            Eq(Var("num2"), Var("num1"))
          ))
        ))
      ))
    performTest(expected, input)
  }


}
