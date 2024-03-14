package inca.ir.clones

import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.extension.string.{TString, StringLit}

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

}
