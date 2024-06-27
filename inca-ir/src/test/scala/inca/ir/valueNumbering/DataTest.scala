package inca.ir.valueNumbering

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string, data}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.extension.data.{Construct, DataDefinition, CaseDefinition, TData}


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


}
