package inca.ir.valueNumbering

import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import inca.ir.extension.{arithmetic, string}
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.arithmetic.*
import inca.ir.*



class BodiesVNTest extends ValueNumberingTestAbstract(){

  test("simple duplicated bodies"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(2))
          ))
        ))
      ))
    val expected =  IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
//          Body(Seq(
//            Eq(Var("result"), IntNum(1))
//          )),
          Body(Seq(
            Eq(Var("result"), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("simple duplicated bodies after VN of terms and atoms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(1)),
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(2)),
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(1))
          //          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(2))
          //          ))
        ))
      ))
    performTest(expected, input)
  }

  test("dont remove bodies of other relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(1)),
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(2)),
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(1))
          //          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(2))
          //          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Duplicated bodies after VN of terms and atoms") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(1)),
          )),
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Eq(Var("temp"), IntNum(1)),
            Eq(Var("result"), Var("temp")),
          )),
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Call("S", Seq(Var("a"))),
            Eq(Var("result"), IntNum(1))
          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(2))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Eq(Var("result"), IntNum(1))
          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(1))
          //          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(2))
          //          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          )),
          Body(Seq(
            Eq(Var("result"), IntNum(2))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Duplicated bodies after VN of terms and atoms (with propagation among relations)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Eq(Var("result"), IntNum(1)),
            Eq(Var("result"), IntNum(1)),
          )),
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Eq(Var("temp"), IntNum(1)),
            Eq(Var("result"), Var("temp")),
          )),
          Body(Seq(
            Call("S", Seq(Var("a"))),
            Call("S", Seq(Var("a"))),
            Eq(Var("result"), IntNum(1))
          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Call("S", Seq(IntNum(1))),
            Eq(Var("result"), IntNum(1))
          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(1))
          //          )),
          //          Body(Seq(
          //            Eq(Var("result"), IntNum(2))
          //          ))
        )),
        Relation(Name("S"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var("result"), IntNum(1))
          ))
        ))
      ))
    performTest(expected, input)
  }


}
