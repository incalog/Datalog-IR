package inca.ir.clones

import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.extension.string.{StringConcat, StringLit, TString}
import inca.ir.string2name


class StringTest extends ValueNumberingTestAbstract {

  test("Simple Redundant string in Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), StringLit("Hello")),
            Eq(Var(Name("Y")), StringLit("Hello")),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("Y")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var(Name("X")), StringLit("Hello")),
            //Eq(Var(Name("Y")), Var(Name("X"))),
            Eq(Var(Name("param$0")), Var(Name("X"))),
            Eq(Var(Name("param$1")), Var(Name("X")))
          ))
        ))
      ))
    performTest(expected,input)
  }

  test("Simple Redundant concat in Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("B")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var(Name("A")), StringConcat(StringLit("Hello"), StringLit(" World"))),
//            Eq(Var(Name("B")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Redundant concat in Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var("H"),StringLit("Hello")),
            Eq(Var(Name("A")), StringConcat(Var("H"), StringLit(" World"))),
            Eq(Var(Name("B")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var("H"),StringLit("Hello")),
            Eq(Var(Name("A")), StringConcat(Var("H"), StringLit(" World"))),
//            Eq(Var(Name("B")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Redundant concat in Eq with different StringLits") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var("H"), StringLit("Hello")),
            Eq(Var(Name("A")), StringConcat(Var("H"), StringLit(" World"))),
            Eq(Var(Name("B")), StringConcat(StringLit("Hell"), StringLit("o World"))), // need to implement simplify function for this
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("B")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", TString), Param("param$1", TString)), Seq(
          Body(Seq(
            Eq(Var("H"), StringLit("Hello")),
            Eq(Var(Name("A")), StringConcat(Var("H"), StringLit(" World"))),
            //            Eq(Var(Name("B")), StringConcat(StringLit("Hello"), StringLit(" World"))),
            Eq(Var(Name("param$0")), Var(Name("A"))),
            Eq(Var(Name("param$1")), Var(Name("A")))
          ))
        ))
      ))
    performTest(expected, input)
  }

}
