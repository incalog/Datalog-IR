package inca.ir.clones

import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Var, Module as IRModule}
import org.scalatest.funsuite.AnyFunSuite
import inca.ir.extension.arithmetic.*
import inca.ir.*
import inca.ir.valueNumbering.{ConfigVN, ValueNumbering}

class OutlineClones extends ValueNumberingTestAbstract {

  test("Repeated Atoms in different Relations") {
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
        Relation(Name("b"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")))),
            Eq(Var(Name("m")),  Mul(IntNum(3), Var("A1"))),
            Eq(Var(Name("result")), Add(Var("m"), Var("n")))
          ))
        )),
        Relation(Name("a"), Seq(Param("n", TInt), Param("result", TInt)), Seq(
          Body(Seq(
            Call(Name("R$0"), Seq(TermArg(Var("A1")),TermArg(Var("n")))),
            Eq(Var(Name("result")), Add(IntNum(1), Var("n")))
          ))
        )),
        Relation(Name("R$0"), Seq(Param("A1", TInt), Param("n", TInt)), Seq(
          Body(Seq(
            Eq(Var(Name("A1")), IntNum(10)),
            Eq(Var(Name("n")),  Mul(IntNum(2), Var("A1")))
          ))
        ))
      ))
    performTest(expected, input, ConfigVN(simplifyArithmetic=true))
  }

}
