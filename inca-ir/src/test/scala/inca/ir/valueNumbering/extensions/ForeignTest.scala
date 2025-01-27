package inca.ir.valueNumbering.extensions

import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign, ForeignLanguage, ForeignTerm, ForeignType}
import inca.ir.valueNumbering.ValueNumberingTestAbstract
import inca.ir.extension.arithmetic.{IntNum, TInt}
import inca.ir.extension.{arithmetic, string}
import inca.ir.{BaseIR, Body, Eq, Language, Name, Param, Relation, Term, Type, Var, Module as IRModule}
import inca.ir.extension.arithmetic.*
import inca.ir.*



class ForeignTest extends ValueNumberingTestAbstract{

  object ForeignLanguageTest extends ForeignLanguage{
    override type Code = String
  }

  case class ForeignTypeTest(name: String) extends ForeignType {
    val code: ForeignLanguageTest.Code = name
    val lang: ForeignLanguageTest.type = ForeignLanguageTest
  }
  object ForeignTypeTest {
    def apply: ForeignTypeTest = ForeignTypeTest("ForeignInt")
    def ForeignInt: ForeignTypeTest = ForeignTypeTest("ForeignInt")
  }

  def asForeignInt(term: Term): ConvertIRForeign = ConvertIRForeign(term, TInt, ForeignTypeTest.ForeignInt)
  def asInt(term: Term): ConvertForeignIR = ConvertForeignIR(term, ForeignTypeTest.ForeignInt, TInt)


  test("Foreign"){
    val input = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", ForeignTypeTest.ForeignInt),
          Param("param$1", TInt), Param("param$2", ForeignTypeTest.ForeignInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), asForeignInt(IntNum(123))),
            Eq(Var("param$1"), asInt(asForeignInt(IntNum(2)))),
            Eq(Var("a"), IntNum(3)),
            Eq(Var("param$2"), asForeignInt(Add(Var("a"), IntNum(2)))),
          ))
        ))
      ))
    val expected = IRModule(Name("Datalog"), Language(Set(new BaseIR {}, new arithmetic.IR {}, new string.IR {})),
      Seq(
        Relation(Name("R"), Seq(Param("param$0", ForeignTypeTest.ForeignInt),
          Param("param$1", TInt), Param("param$2", ForeignTypeTest.ForeignInt)), Seq(
          Body(Seq(
            Eq(Var("param$0"), asForeignInt(IntNum(123))),
            Eq(Var("param$1"), asInt(asForeignInt(IntNum(2)))),
//            Eq(Var("a"), IntNum(3)),
            Eq(Var("param$2"), asForeignInt(IntNum(5))),
          ))
        ))
      ))
    performTest(expected, input)
  }

}
