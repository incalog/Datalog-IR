package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.valueNumbering.{ConfigVN, ValueNumbering}

class OutlineClonesTest extends ValueNumberingTestAbstract {

  override val config: ConfigVN = ConfigVN(normalize=true, outline=true)

  test("2 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")),  Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")))),
            Eq(Var(Name("m")),  Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("2 repeated atoms in 3 different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), Mul(IntNum(5), IntNum(3))),
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), Var("C1")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), IntNum(15)),
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(Var("C1"),Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("2 repeated atoms in different relations (equality based on prev VN)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("A2")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A2"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("3 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("A2")), Mul(IntNum(10), Add(IntNum(3), IntNum(5)))),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("A2")), Mul(IntNum(10), Add(IntNum(3), IntNum(5)))),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("A2", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("A2")), IntNum(80)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("A2")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("A2")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("2 repeated atoms in different bodies (same relation)") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          )),
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          )),
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("2 * 2 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("d"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("m")), Mul(Var("C1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("C1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), IntNum(5)),
            Eq(Var(Name("n")), IntNum(2)),
          ))
        )),
        Relation(Name("R$1"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$1"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$1"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3),Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"),Var("n")))
          ))
        )),
        Relation(Name("c"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("C1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("d"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("C1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3),Var("C1"))),
            Eq(Var(Name("result")), Add(Var("m"),Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("4 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n2")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n2")), IntNum(2)),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt), Param("C1", TInt), Param("n2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")),  Mul(IntNum(2), Var("A1"))),
            Eq(Var(Name("C1")), IntNum(5)),
            Eq(Var(Name("n2")), IntNum(2)),
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")),TermArg(Var("C1")),TermArg(Var("n2")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")),TermArg(Var("C1")),TermArg(Var("n2")))),
            Eq(Var(Name("m")),  Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("2*2 repeated atoms in same bodies") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("A2")), IntNum(1010)),
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n2")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n2")), IntNum(2)),
            Eq(Var(Name("m")), Mul(Var("A1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1"))),
          ))
        )),
        Relation(Name("R$1"), Seq(Param("C1", TInt), Param("n2", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), IntNum(5)),
            Eq(Var(Name("n2")), IntNum(2)),
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("A2")), IntNum(1010)),
            Call(Name("R$1"), Seq(TermArg(Var("C1")), TermArg(Var("n2")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Call(Name("R$1"), Seq(TermArg(Var("C1")), TermArg(Var("n2")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("repeated atoms with 'gap' in equality") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("A2")), IntNum(5)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("result")), Add(Var("n"), Var("A2")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("A2")), IntNum(1)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("m")), Mul(IntNum(11),  Var("A2"))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("A2")), IntNum(5)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("result")), Add(Var("A2"),Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("A2")), IntNum(1)),
            Eq(Var(Name("n")), Mul(Var("A1"), Var("A2"))),
            Eq(Var(Name("m")), Mul(IntNum(11),  Var("A2"))),
            Eq(Var(Name("result")), Add(Var("m"),Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Vars with same names but different values") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(3), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(IntNum(4),Var("A1"))),
            Eq(Var(Name("m")), Mul(IntNum(5),Var("A1"))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2),Var("A1"))),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(15)),
            Eq(Var(Name("n")), Mul(IntNum(4),Var("A1"))),
            Eq(Var(Name("m")), Mul(IntNum(5),Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"),Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }


  test("Vars with different names: 2 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Eq(Var(Name("n")), Mul(Var("B1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("B1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("Vars with different names in Call: 2 repeated atoms in different relations") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Call(Name("c"), Seq(TermArg(Var("A2")))),
            Eq(Var(Name("n")), Mul(Var("A1"), IntNum(2))),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("B1")), Mul(IntNum(2), Add(IntNum(2), IntNum(3)))),
            Call(Name("c"), Seq(TermArg(Var("B2")))),
            Eq(Var(Name("n")), Mul(Var("B1"), IntNum(2))),
            Eq(Var(Name("m")), Mul(Var("B1"), IntNum(3))),
            Eq(Var(Name("result")), Add(Var("n"), Var("m")))
          ))
        )),
        Relation(Name("c"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), Add(IntNum(2), IntNum(3))),
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("A2", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Call(Name("c"), Seq(TermArg(Var("A2")))),
            Eq(Var(Name("n")), Mul(IntNum(2), Var("A1")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("A2")), TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")), TermArg(Var("A2")), TermArg(Var("n")))),
            Eq(Var(Name("m")), Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        )),
        Relation(Name("c"), Seq(Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("C1")), IntNum(5)),
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("result")), Add(IntNum(1),Var("n")))
          ))
        ))
      ))
    performTest(expected, input)
  }

  test("equal params with different names") {
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("b"), Seq(Param("m", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("abc")), IntNum(5)),
            Eq(Var(Name("m")), IntNum(2)),
            Eq(Var(Name("result")), Add(IntNum(1), Var("m")))
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("n")), IntNum(2)),
            Eq(Var(Name("result")), Add(Var("n"), IntNum(1)))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("n")), TermArg(Var("result")))),
          ))
        )),
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("abc")), IntNum(5)),
            Call(Name("R$0"), Seq(TermArg(Var("n")), TermArg(Var("result")))),
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(normalize=true, outline=true, attemptAlphaEquivalence=true))
  }
  // TODO when both relations are alpha equiv. then the removed is still counted for occurences of atoms

}
