package inca.ir.valueNumbering

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*


class RelationsVNTest extends ValueNumberingTestAbstract {

  test("Simple duplicated Relation"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        ))
      ))
    performTest(expected, input)
  }

  test("Simple duplicated Relation 3-times") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        ))
      ))
    performTest(expected, input)
  }

  test("Simple duplicated Relation after VN of terms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), Var("b")),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        ))
      ))
    performTest(expected, input)
  }

  test("Simple duplicated Relation after VN of terms and atoms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), Var("b")),
            Eq(Var("a"), Var("b")),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        ))
      ))
    performTest(expected, input)
  }

  test("Simple duplicated Relation after VN of terms, atoms and bodies") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), Var("b")),
            Eq(Var("a"), Var("b")),
          )),
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        ))
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation after VN of terms and global propagation") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("T", Seq(TermArg(IntNum(0)))),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("T", Seq(TermArg(Var("b")))),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Eq(Var("res"), IntNum(0)),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Call("T", Seq(TermArg(IntNum(0)))),
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        ))
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Eq(Var("res"), IntNum(0)),
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation: replace names in Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(0)),
          )),
          Body(Seq(
           Call("S", Seq(TermArg(Var("res"))))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(IntNum(0)))),
            Eq(Var("res"), IntNum(0)),
          )),
          Body(Seq(
            Call("R", Seq(TermArg(Var("res"))))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation: replace names in Calls 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Call("S", Seq(TermArg(IntNum(0))))
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Call("S", Seq(TermArg(IntNum(0))))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Call("R", Seq(TermArg(IntNum(0))))
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //            Call("S", Seq(TermArg(IntNum(0))))
        //          ))
        //        )),
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation: replace names in Calls (calls before relation)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(0)),
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(IntNum(0)))),
            Eq(Var("res"), IntNum(0)),
          )),
          Body(Seq(
            Call("R", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        )),
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation: replace names in Calls (calls before relation & discovered in repetition phase)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("V"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("T", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(123)),
          )),
          Body(Seq(
            Call("U", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(1)),
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("U"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(1)),
          )),
          Body(Seq(
            Call("S", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("V"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("T", Seq(TermArg(Var("a")))),
            Eq(Var("res"), IntNum(123)),
          )),
          Body(Seq(
            Call("T", Seq(TermArg(Var("res"))))
          ))
        )),
        Relation(Name("T"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(IntNum(0)))),
            Eq(Var("res"), IntNum(1)),
          )),
          Body(Seq(
            Call("R", Seq(TermArg(Var("res"))))
          ))
        )),
//        Relation(Name("U"), Seq(Param("res", TInt)), Seq(
//          Body(Seq(
//            Call("S", Seq(TermArg(Var("a")))),
//            Eq(Var("res"), IntNum(1)),
//          )),
//          Body(Seq(
//            Call("S", Seq(TermArg(Var("res"))))
//          ))
//        )),
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
      ))
    performTest(expected, input)
  }


  test("Duplicated Relation 3-times: replace names in Calls") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("U"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Call("R", Seq(TermArg(Var("b")))),
            Call("S", Seq(TermArg(Var("c")))),
            Call("T", Seq(TermArg(Var("d")))),
          ))
        )),
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
//        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
        Relation(Name("U"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
            Call("R", Seq(TermArg(IntNum(0)))),
//            Call("S", Seq(TermArg(Var("c")))),
//            Call("T", Seq(TermArg(Var("d")))),
          ))
        )),
      ))
    performTest(expected, input)
  }

  test("Duplicated Relation 3-times: replace names in Calls 2") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        Relation(Name("U"), Seq(Param("a", TInt),Param("b", TInt),Param("c", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("a")))),
            Call("S", Seq(TermArg(Var("b")))),
            Call("T", Seq(TermArg(Var("c")))),
          ))
        )),
        Relation(Name("V"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Eq(Var("res"), IntNum(1)),
            Call("U", Seq(TermArg(Var("a")),TermArg(Var("b")),TermArg(Var("c")))),
            Call("R", Seq(TermArg(Var("a")))),
            Call("S", Seq(TermArg(Var("b")))),
            Call("T", Seq(TermArg(Var("c")))),
          ))
        )),
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
          Body(Seq(
            Eq(Var("a"), IntNum(0)),
          ))
        )),
        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        )),
        //        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
        //          Body(Seq(
        //            Eq(Var("a"), IntNum(0)),
        //          ))
        //        )),
        Relation(Name("U"), Seq(Param("a", TInt),Param("b", TInt),Param("c", TInt)), Seq(
          Body(Seq(
            Call("R", Seq(TermArg(Var("a")))),
            Call("R", Seq(TermArg(Var("b")))),
            Call("R", Seq(TermArg(Var("c")))),
          ))
        )),
        Relation(Name("V"), Seq(Param("res", TInt)), Seq(
          Body(Seq(
            Eq(Var("res"), IntNum(1)),
            Call("U", Seq(TermArg(IntNum(0)),TermArg(IntNum(0)),TermArg(IntNum(0)))),
            Call("R", Seq(TermArg(IntNum(0)))),
//            Call("R", Seq(TermArg(Var("b")))),
//            Call("R", Seq(TermArg(Var("c")))),
          ))
        )),
      ))
    performTest(expected, input)
  }

  // should not be a problem <- because of typesystem every variable needs to be bound and var with different type cannot be bound in the same way
  test("Duplicated Relation: same relation but different types") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("EqInt"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), Var("b"))
          ))
        )),
        Relation(Name("EqDouble"), Seq(Param("a", TDouble), Param("b", TDouble)), Seq(
          Body(Seq(
            Eq(Var("b"), DoubleNum(0)),
            Eq(Var("a"), Var("b"))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("EqInt"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
          Body(Seq(
            Eq(Var("b"), IntNum(0)),
            Eq(Var("a"), Var("b"))
          ))
        )),
        Relation(Name("EqDouble"), Seq(Param("a", TDouble), Param("b", TDouble)), Seq(
          Body(Seq(
            Eq(Var("b"), DoubleNum(0)),
            Eq(Var("a"), Var("b"))
          ))
        ))
      ))
    performTest(expected, input)
  }

//  test("Duplicated Relation (alpha equivalence: parameters)") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
//        Relation(Name("S"), Seq(Param("b", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("b"), IntNum(0)),
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          ))
//        )),
//        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//        //          Body(Seq(
//        //            Eq(Var("a"), IntNum(0)),
//        //          ))
//        //        ))
//      ))
//    performTest(expected, input)
//  }

//  test("Duplicated Relation (alpha equivalence: other variables)") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Eq(Var("b"), IntNum(1)),
//          ))
//        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Eq(Var("c"), IntNum(1)),
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
////            Eq(Var("b"), IntNum(1)),
//          ))
//        )),
//        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//        //          Body(Seq(
//        //            Eq(Var("a"), IntNum(0)),
//        //          ))
//        //        ))
//      ))
//    performTest(expected, input)
//  }

//  test("Duplicated Relation (alpha equivalence: other variables 2)") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Call("T", Seq(TermArg(Var("b"))))
//          ))
//        )),
//        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Call("T", Seq(TermArg(Var("c"))))
//          ))
//        )),
//        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          )),
//          Body(Seq(
//            Eq(Var("a"), IntNum(1)),
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Call("T", Seq(TermArg(Var("b"))))
//          ))
//        )),
////        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
////          Body(Seq(
////            Eq(Var("a"), IntNum(0)),
////            Call("T", Seq(TermArg(Var("c"))))
////          ))
////        )),
//        Relation(Name("T"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//          )),
//          Body(Seq(
//            Eq(Var("a"), IntNum(1)),
//          ))
//        ))
//      ))
//    performTest(expected, input)
//  }

//  test("Duplicated Relation (parameters different order)") {
//    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt), Param("b", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Eq(Var("b"), IntNum(1))
//          ))
//        )),
//        Relation(Name("S"), Seq(Param("b", TInt), Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Eq(Var("b"), IntNum(1)),
//          ))
//        ))
//      ))
//    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
//      Seq(
//        Relation(Name("R"), Seq(Param("a", TInt)), Seq(
//          Body(Seq(
//            Eq(Var("a"), IntNum(0)),
//            Eq(Var("b"), IntNum(1))
//          ))
//        )),
//        //        Relation(Name("S"), Seq(Param("a", TInt)), Seq(
//        //          Body(Seq(
//        //            Eq(Var("a"), IntNum(0)),
//        //          ))
//        //        ))
//      ))
//    performTest(expected, input)
//  }

}
