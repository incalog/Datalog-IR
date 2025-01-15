package inca.ir.valueNumbering

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*

class AtomsVNTest extends ValueNumberingTestAbstract(){

  test("simple duplicated atoms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Add(Var("a"), IntNum(2))),
            Eq(Var("b"), Add(Var("a"), IntNum(2))),
            Eq(Var("result"), Add(Var("a"), Var("b"))),
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
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
//            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Add(IntNum(2),Var("a"))),
//            Eq(Var("b"), Add(Var("a"), IntNum(2))),
            Eq(Var("result"), Add(Var("a"), Var("b"))),
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

  test("duplicated atoms (found with VN of terms)"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Eq(Var("H1"), Add(Var("a"), IntNum(2))), // was previously not removed (even with fix-point iteration) because second Eq that is now binding H2 is comparison with unknown Var on rhs)
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
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("b")))),
//            Call(Name("S1"), Seq(TermArg(Var("b")))),
            Eq(Var("H2"), Add(IntNum(2), Var("b"))),
//            Eq(Var("a"), Var("b")),
//            Eq(Var("H2"), Add(Var("b"), IntNum(2))),
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
        ))
      ))
    performTest(expected, input)
  }

  test("duplicated atoms (found with normalization of atoms)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Add(Var("a"), IntNum(2))),
            Eq(Add(Var("a"), IntNum(2)), Var("b")),
            Eq(Add(Var("a"), Var("b")), Var("result")),
            Eq(Var("result"), Add(Var("a"), Var("b"))),
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
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            //            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("b"), Add(IntNum(2), Var("a"))),
            //            Eq(Var("b"), Add(Var("a"), IntNum(2))),
            Eq(Var("result"), Add(Var("a"), Var("b"))),
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

  test("Negated Eq") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("a")))),
            Eq(Var("a"), Var("a"), true),
            Eq(Var("result"), Add(Var("a"), Var("a"))),
          )),
          Body(Seq(
            Call(Name("S1"), Seq(TermArg(Var("result")))),
            Eq(IntNum(1), IntNum(1), true),
          )),
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
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
//          Body(Seq(
//            Call(Name("S1"), Seq(TermArg(Var("a")))),
//            Eq(Var("a"), Var("a"), true),
//            Eq(Var("result"), Add(Var("a"), Var("a"))),
//          )),
//          Body(Seq(
//            Call(Name("S1"), Seq(TermArg(Var("result")))),
//            Eq(IntNum(1), IntNum(1), true),
//          )),
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
            Call("S", Seq(TermArg(IntNum(0)), TermArg(IntNum(1))), true),
            Call("S", Seq(TermArg(IntNum(0)), TermArg(IntNum(1)))),
            Call("S", Seq(TermArg(Var("c")), TermArg(Var("c"))), true),
            Call("S", Seq(TermArg(Var("c")), TermArg(Var("c"))), true),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
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
            Call("S", Seq(TermArg(IntNum(0)), TermArg(IntNum(1))), true),
            Call("S", Seq(TermArg(IntNum(0)), TermArg(IntNum(1)))),
            Call("S", Seq(TermArg(Var("c")), TermArg(Var("c"))), true),
//            Call("S", Seq(TermArg(Var("c")), TermArg(Var("c"))), true),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
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

}
